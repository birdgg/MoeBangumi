use std::collections::HashSet;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use futures::{stream, StreamExt};
use sqlx::SqlitePool;
use tokio::sync::{mpsc, Semaphore};
use tokio::task::JoinHandle;
use tokio::time::MissedTickBehavior;

use super::super::poster::PosterService;
use super::messages::{MetadataMessage, SyncStats};
use crate::repositories::{MetadataRepository, MetadataToSync};

/// Poster 下载并发限制
const POSTER_CONCURRENCY: usize = 10;

/// 同步记录处理并发限制（限制同时处理的记录数量，防止内存占用过高）
/// 实际的 Poster 下载并发由 POSTER_CONCURRENCY 信号量控制
const SYNC_CONCURRENCY: usize = 5;

/// 批量数据库更新间隔
const BATCH_UPDATE_INTERVAL: Duration = Duration::from_millis(500);

/// 批量更新缓冲区大小（超过此数量立即写入）
const BATCH_UPDATE_BUFFER_SIZE: usize = 20;

/// RAII guard 确保 URL 在任务完成或 panic 时从去重集合中移除
struct UrlGuard {
    url: String,
    in_progress: Arc<Mutex<HashSet<String>>>,
}

impl Drop for UrlGuard {
    fn drop(&mut self) {
        match self.in_progress.lock() {
            Ok(mut set) => {
                set.remove(&self.url);
            }
            Err(poisoned) => {
                tracing::warn!("Mutex poisoned in UrlGuard::drop for URL: {}", self.url);
                let mut set = poisoned.into_inner();
                set.remove(&self.url);
            }
        }
    }
}

/// 尝试注册 URL 进行下载（去重检查）
///
/// 如果 URL 已在下载中，返回 false；否则注册并返回 true。
/// 使用 `into_inner()` 从污染的 Mutex 中恢复，避免级联 panic。
fn try_register_url(url: &str, in_progress: &Mutex<HashSet<String>>) -> bool {
    let mut set = match in_progress.lock() {
        Ok(guard) => guard,
        Err(poisoned) => {
            // Recover the data without clearing - panic typically indicates logic error,
            // not data corruption. Keeping the set preserves deduplication protection.
            tracing::error!(
                "Mutex poisoned in try_register_url, recovering with existing data (set size: {})",
                poisoned.get_ref().len()
            );
            poisoned.into_inner()
        }
    };
    if set.contains(url) {
        false
    } else {
        set.insert(url.to_string());
        true
    }
}

/// 提交海报下载任务（共享实现）
///
/// 返回 true 表示已提交下载任务，false 表示跳过（重复或已是本地路径）。
/// 注意：本地路径检查由 `PosterService::download_from_url()` 内部处理，
/// 以支持可配置的 URL 前缀。
fn spawn_poster_download_task(
    metadata_id: i64,
    url: String,
    poster_service: Arc<PosterService>,
    semaphore: Arc<Semaphore>,
    in_progress_urls: Arc<Mutex<HashSet<String>>>,
    update_sender: mpsc::UnboundedSender<(i64, String)>,
) -> bool {
    // 去重检查
    if !try_register_url(&url, &in_progress_urls) {
        tracing::debug!(
            "Skipping duplicate poster download for metadata_id={}: {}",
            metadata_id,
            url
        );
        return false;
    }

    tokio::spawn(async move {
        let _guard = UrlGuard {
            url: url.clone(),
            in_progress: in_progress_urls,
        };
        let _permit = match semaphore.acquire().await {
            Ok(p) => p,
            Err(_) => return,
        };

        tracing::debug!(
            "Downloading poster for metadata_id={}: {}",
            metadata_id,
            url
        );

        match poster_service.download_from_url(&url).await {
            Ok(local_path) => {
                // 发送到批量更新 channel，而不是立即更新数据库
                if let Err(e) = update_sender.send((metadata_id, local_path)) {
                    tracing::error!(
                        "Failed to queue poster update for metadata {}: {}",
                        metadata_id,
                        e
                    );
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
    });

    true
}

/// 批量数据库更新后台任务
///
/// 收集下载完成的海报更新请求，定期批量写入数据库
fn spawn_batch_update_task(
    db: SqlitePool,
    mut update_receiver: mpsc::UnboundedReceiver<(i64, String)>,
) -> JoinHandle<()> {
    tokio::spawn(async move {
        let mut buffer: Vec<(i64, String)> = Vec::with_capacity(BATCH_UPDATE_BUFFER_SIZE);
        let mut ticker = tokio::time::interval(BATCH_UPDATE_INTERVAL);
        ticker.set_missed_tick_behavior(MissedTickBehavior::Skip);

        loop {
            tokio::select! {
                // 接收更新请求
                result = update_receiver.recv() => {
                    match result {
                        Some(update) => {
                            buffer.push(update);

                            // 缓冲区满了立即刷新
                            if buffer.len() >= BATCH_UPDATE_BUFFER_SIZE {
                                flush_updates(&db, &mut buffer).await;
                            }
                        }
                        None => {
                            // Channel 关闭，刷新剩余数据后退出
                            if !buffer.is_empty() {
                                flush_updates(&db, &mut buffer).await;
                            }
                            tracing::debug!("Batch update task stopped");
                            break;
                        }
                    }
                }

                // 定时刷新
                _ = ticker.tick() => {
                    if !buffer.is_empty() {
                        flush_updates(&db, &mut buffer).await;
                    }
                }
            }
        }
    })
}

/// 刷新缓冲区到数据库
async fn flush_updates(db: &SqlitePool, buffer: &mut Vec<(i64, String)>) {
    let count = buffer.len();

    match MetadataRepository::batch_update_poster_urls(db, buffer).await {
        Ok(affected) => {
            tracing::debug!(
                "Batch updated {} poster URLs ({} rows affected)",
                count,
                affected
            );
        }
        Err(e) => {
            tracing::error!("Failed to batch update poster URLs: {}", e);
        }
    }

    buffer.clear();
}

/// Metadata Actor
///
/// 处理 Poster 下载功能。
/// 内置定时器自动执行同步任务。
pub struct MetadataActor {
    db: SqlitePool,
    poster_service: Arc<PosterService>,
    receiver: mpsc::Receiver<MetadataMessage>,
    poster_semaphore: Arc<Semaphore>,
    in_progress_urls: Arc<Mutex<HashSet<String>>>,
    sync_interval: Duration,
    update_sender: mpsc::UnboundedSender<(i64, String)>,
    batch_update_task: JoinHandle<()>,
}

impl MetadataActor {
    pub fn new(
        db: SqlitePool,
        poster_service: Arc<PosterService>,
        receiver: mpsc::Receiver<MetadataMessage>,
        sync_interval: Duration,
    ) -> Self {
        let (update_sender, update_receiver) = mpsc::unbounded_channel();

        // 启动批量写入后台任务
        let batch_update_task = spawn_batch_update_task(db.clone(), update_receiver);

        Self {
            db,
            poster_service,
            receiver,
            poster_semaphore: Arc::new(Semaphore::new(POSTER_CONCURRENCY)),
            in_progress_urls: Arc::new(Mutex::new(HashSet::new())),
            sync_interval,
            update_sender,
            batch_update_task,
        }
    }

    /// 运行 Actor 主循环
    pub async fn run(mut self) {
        // 启动定时同步任务
        let sync_task = self.spawn_sync_interval();

        // Actor 消息循环
        while let Some(msg) = self.receiver.recv().await {
            if !self.handle_message(msg).await {
                break;
            }
        }

        // Actor 停止时取消定时任务和批量更新任务
        sync_task.abort();
        self.batch_update_task.abort();
        tracing::info!("Metadata actor stopped");
    }

    /// 启动定时同步任务 (内部 interval)
    fn spawn_sync_interval(&self) -> JoinHandle<()> {
        let db = self.db.clone();
        let poster_service = Arc::clone(&self.poster_service);
        let poster_semaphore = Arc::clone(&self.poster_semaphore);
        let in_progress_urls = Arc::clone(&self.in_progress_urls);
        let update_sender = self.update_sender.clone();
        let interval_duration = self.sync_interval;

        tokio::spawn(async move {
            let mut ticker = tokio::time::interval(interval_duration);
            ticker.set_missed_tick_behavior(MissedTickBehavior::Skip);

            // 跳过第一次立即触发
            ticker.tick().await;

            loop {
                ticker.tick().await;

                let _ = sync_all_metadata(
                    db.clone(),
                    Arc::clone(&poster_service),
                    Arc::clone(&poster_semaphore),
                    Arc::clone(&in_progress_urls),
                    update_sender.clone(),
                )
                .await;
            }
        })
    }

    /// 处理消息
    ///
    /// 返回 `true` 继续处理消息，返回 `false` 停止 Actor。
    async fn handle_message(&self, msg: MetadataMessage) -> bool {
        match msg {
            MetadataMessage::DownloadPoster { metadata_id, url } => {
                self.spawn_poster_download(metadata_id, url);
            }
            MetadataMessage::DownloadPosterBatch { tasks } => {
                for (metadata_id, url) in tasks {
                    self.spawn_poster_download(metadata_id, url);
                }
            }
            MetadataMessage::TriggerSync => {
                tracing::info!("Manual metadata sync triggered");

                let _ = sync_all_metadata(
                    self.db.clone(),
                    Arc::clone(&self.poster_service),
                    Arc::clone(&self.poster_semaphore),
                    Arc::clone(&self.in_progress_urls),
                    self.update_sender.clone(),
                )
                .await;
            }
            MetadataMessage::Shutdown => {
                tracing::info!("Metadata actor received shutdown signal");
                return false;
            }
        }
        true
    }

    /// 异步提交 Poster 下载任务
    fn spawn_poster_download(&self, metadata_id: i64, url: String) {
        spawn_poster_download_task(
            metadata_id,
            url,
            Arc::clone(&self.poster_service),
            Arc::clone(&self.poster_semaphore),
            Arc::clone(&self.in_progress_urls),
            self.update_sender.clone(),
        );
    }
}

/// 每次同步处理的记录数
const SYNC_CHUNK_SIZE: i64 = 100;

/// 执行完整的海报同步（分块处理）
async fn sync_all_metadata(
    db: SqlitePool,
    poster_service: Arc<PosterService>,
    poster_semaphore: Arc<Semaphore>,
    in_progress_urls: Arc<Mutex<HashSet<String>>>,
    update_sender: mpsc::UnboundedSender<(i64, String)>,
) -> SyncStats {
    // Get total count first
    let total_count = match MetadataRepository::count_metadata_to_sync(&db).await {
        Ok(count) => count,
        Err(e) => {
            tracing::error!("Failed to count metadata to sync: {}", e);
            return SyncStats::default();
        }
    };

    if total_count == 0 {
        tracing::debug!("No metadata to sync");
        return SyncStats::default();
    }

    tracing::info!("Found {} metadata records to sync", total_count);

    let mut stats = SyncStats::default();
    let mut last_id: i64 = 0;
    let mut chunk_num: i64 = 0;

    // Process in chunks using keyset pagination (seek method)
    // Each iteration fetches records with id > last_id, ensuring each record
    // is processed exactly once per sync cycle.
    loop {
        let records =
            match MetadataRepository::get_metadata_to_sync(&db, SYNC_CHUNK_SIZE, last_id).await {
                Ok(r) => r,
                Err(e) => {
                    tracing::error!("Failed to get metadata chunk after id {}: {}", last_id, e);
                    break;
                }
            };

        if records.is_empty() {
            break;
        }

        chunk_num += 1;
        let chunk_size = records.len();

        // Update last_id for next iteration
        if let Some(last) = records.last() {
            last_id = last.id;
        }

        tracing::debug!(
            "Processing chunk {} ({} poster records)",
            chunk_num,
            chunk_size
        );

        let results: Vec<bool> = stream::iter(records)
            .map(|record| {
                let poster = Arc::clone(&poster_service);
                let semaphore = Arc::clone(&poster_semaphore);
                let in_progress = Arc::clone(&in_progress_urls);
                let sender = update_sender.clone();
                async move { process_sync_record(record, poster, semaphore, in_progress, sender).await }
            })
            .buffer_unordered(SYNC_CONCURRENCY)
            .collect()
            .await;

        for queued in results {
            if queued {
                stats.posters_queued += 1;
            }
        }

        // If we got fewer records than requested, we've reached the end
        if (chunk_size as i64) < SYNC_CHUNK_SIZE {
            break;
        }
    }

    stats
}

/// 处理单条同步记录
async fn process_sync_record(
    record: MetadataToSync,
    poster: Arc<PosterService>,
    semaphore: Arc<Semaphore>,
    in_progress_urls: Arc<Mutex<HashSet<String>>>,
    update_sender: mpsc::UnboundedSender<(i64, String)>,
) -> bool {
    // poster_url is guaranteed to be Some and remote URL by the query
    let url = record.poster_url.unwrap();
    let queued = spawn_poster_download_task(
        record.id,
        url,
        poster,
        semaphore,
        in_progress_urls,
        update_sender,
    );

    if queued {
        tracing::info!("{} poster queued for download", record.title_chinese);
    }

    queued
}
