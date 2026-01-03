use std::collections::HashSet;
use std::sync::Arc;

use sqlx::SqlitePool;
use tokio::sync::{mpsc, Mutex, Semaphore};

use super::messages::{DownloadTask, PosterDownloadMessage};
use crate::repositories::MetadataRepository;
use crate::services::poster::PosterService;

/// 并发下载限制
const CONCURRENCY_LIMIT: usize = 5;

/// RAII guard 确保 URL 在任务完成或 panic 时从去重集合中移除
struct UrlGuard {
    url: String,
    in_progress: Arc<Mutex<HashSet<String>>>,
}

impl Drop for UrlGuard {
    fn drop(&mut self) {
        let url = std::mem::take(&mut self.url);
        let in_progress = Arc::clone(&self.in_progress);
        // spawn 一个任务来清理，因为 drop 不能是 async
        tokio::spawn(async move {
            in_progress.lock().await.remove(&url);
        });
    }
}

/// Poster 下载 Actor
///
/// 负责异步下载图片并更新 metadata 的 poster_url。
/// 使用 Semaphore 控制并发数，HashSet 防止重复下载。
pub struct PosterDownloadActor {
    db: SqlitePool,
    poster: Arc<PosterService>,
    receiver: mpsc::Receiver<PosterDownloadMessage>,
    semaphore: Arc<Semaphore>,
    /// 正在下载的 URL 集合（用于去重）
    in_progress: Arc<Mutex<HashSet<String>>>,
}

impl PosterDownloadActor {
    pub fn new(
        db: SqlitePool,
        poster: Arc<PosterService>,
        receiver: mpsc::Receiver<PosterDownloadMessage>,
    ) -> Self {
        Self {
            db,
            poster,
            receiver,
            semaphore: Arc::new(Semaphore::new(CONCURRENCY_LIMIT)),
            in_progress: Arc::new(Mutex::new(HashSet::new())),
        }
    }

    /// 运行 Actor 主循环
    pub async fn run(mut self) {
        tracing::info!("PosterDownload actor started");

        while let Some(msg) = self.receiver.recv().await {
            self.handle_message(msg).await;
        }

        tracing::info!("PosterDownload actor stopped");
    }

    /// 处理消息
    async fn handle_message(&mut self, msg: PosterDownloadMessage) {
        match msg {
            PosterDownloadMessage::Download(task) => {
                self.spawn_download_task(task).await;
            }
        }
    }

    /// 异步提交下载任务（使用 Semaphore 控制并发）
    async fn spawn_download_task(&self, task: DownloadTask) {
        let url = task.url.clone();

        // 去重检查并标记
        {
            let mut in_progress = self.in_progress.lock().await;
            if in_progress.contains(&url) {
                tracing::debug!(
                    "Skipping download for metadata_id={} (already in progress): {}",
                    task.metadata_id,
                    url
                );
                return;
            }
            in_progress.insert(url.clone());
        }

        // 克隆需要的资源
        let db = self.db.clone();
        let poster = Arc::clone(&self.poster);
        let semaphore = Arc::clone(&self.semaphore);
        let in_progress = Arc::clone(&self.in_progress);
        let metadata_id = task.metadata_id;

        // Spawn 异步任务
        tokio::spawn(async move {
            // 创建 RAII guard，确保无论如何都会清理 URL
            let _guard = UrlGuard {
                url: url.clone(),
                in_progress,
            };

            // 获取 permit（阻塞直到有空闲槽）
            let _permit = match semaphore.acquire().await {
                Ok(permit) => permit,
                Err(_) => {
                    // Semaphore closed
                    return;
                }
            };

            tracing::debug!(
                "Starting poster download for metadata_id={}: {}",
                metadata_id,
                url
            );

            // 执行下载
            match poster.download_from_url(&url).await {
                Ok(local_path) => {
                    // 更新数据库
                    match MetadataRepository::update_poster_url(&db, metadata_id, &local_path).await
                    {
                        Ok(true) => {
                            tracing::info!(
                                "Poster downloaded for metadata_id={}: {} -> {}",
                                metadata_id,
                                url,
                                local_path
                            );
                        }
                        Ok(false) => {
                            tracing::warn!(
                                "Metadata {} not found when updating poster_url",
                                metadata_id
                            );
                        }
                        Err(e) => {
                            tracing::error!(
                                "Failed to update poster_url for metadata {}: {}",
                                metadata_id,
                                e
                            );
                        }
                    }
                }
                Err(e) => {
                    tracing::warn!(
                        "Failed to download poster for metadata {}: {}",
                        metadata_id,
                        e
                    );
                }
            }

            // _guard 在这里 drop，自动清理 URL
            // _permit 在这里 drop，自动释放 semaphore slot
        });
    }
}
