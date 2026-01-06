use std::collections::HashSet;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use futures::{stream, StreamExt};
use sqlx::SqlitePool;
use tokio::sync::{mpsc, Semaphore};
use tokio::task::JoinHandle;
use tokio::time::MissedTickBehavior;

use super::super::poster::PosterService;
use super::super::service::MetadataService;
use super::messages::{MetadataMessage, SyncStats};
use crate::repositories::{MetadataRepository, MetadataToSync};

/// Poster 下载并发限制
const POSTER_CONCURRENCY: usize = 5;

/// Metadata 同步并发限制
const SYNC_CONCURRENCY: usize = 5;

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
    db: SqlitePool,
    poster_service: Arc<PosterService>,
    semaphore: Arc<Semaphore>,
    in_progress_urls: Arc<Mutex<HashSet<String>>>,
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
                if let Err(e) =
                    MetadataRepository::update_poster_url(&db, metadata_id, &local_path).await
                {
                    tracing::error!(
                        "Failed to update poster_url for metadata {}: {}",
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

/// Metadata Actor
///
/// 整合 Poster 下载 + TMDB 同步功能。
/// 内置定时器自动执行同步任务。
pub struct MetadataActor {
    db: SqlitePool,
    metadata_service: Arc<MetadataService>,
    poster_service: Arc<PosterService>,
    receiver: mpsc::Receiver<MetadataMessage>,
    poster_semaphore: Arc<Semaphore>,
    in_progress_urls: Arc<Mutex<HashSet<String>>>,
    sync_interval: Duration,
}

impl MetadataActor {
    pub fn new(
        db: SqlitePool,
        metadata_service: Arc<MetadataService>,
        poster_service: Arc<PosterService>,
        receiver: mpsc::Receiver<MetadataMessage>,
        sync_interval: Duration,
    ) -> Self {
        Self {
            db,
            metadata_service,
            poster_service,
            receiver,
            poster_semaphore: Arc::new(Semaphore::new(POSTER_CONCURRENCY)),
            in_progress_urls: Arc::new(Mutex::new(HashSet::new())),
            sync_interval,
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

        // Actor 停止时取消定时任务
        sync_task.abort();
        tracing::info!("Metadata actor stopped");
    }

    /// 启动定时同步任务 (内部 interval)
    fn spawn_sync_interval(&self) -> JoinHandle<()> {
        let db = self.db.clone();
        let metadata_service = Arc::clone(&self.metadata_service);
        let poster_service = Arc::clone(&self.poster_service);
        let poster_semaphore = Arc::clone(&self.poster_semaphore);
        let in_progress_urls = Arc::clone(&self.in_progress_urls);
        let interval_duration = self.sync_interval;

        tokio::spawn(async move {
            let mut ticker = tokio::time::interval(interval_duration);
            ticker.set_missed_tick_behavior(MissedTickBehavior::Skip);

            // 跳过第一次立即触发
            ticker.tick().await;

            loop {
                ticker.tick().await;

                tracing::info!("Metadata interval sync triggered");

                let _ = sync_all_metadata(
                    db.clone(),
                    Arc::clone(&metadata_service),
                    Arc::clone(&poster_service),
                    Arc::clone(&poster_semaphore),
                    Arc::clone(&in_progress_urls),
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
                    Arc::clone(&self.metadata_service),
                    Arc::clone(&self.poster_service),
                    Arc::clone(&self.poster_semaphore),
                    Arc::clone(&self.in_progress_urls),
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
            self.db.clone(),
            Arc::clone(&self.poster_service),
            Arc::clone(&self.poster_semaphore),
            Arc::clone(&self.in_progress_urls),
        );
    }
}

/// 每次同步处理的记录数
const SYNC_CHUNK_SIZE: i64 = 100;

/// 执行完整的元数据同步（分块处理）
async fn sync_all_metadata(
    db: SqlitePool,
    metadata_service: Arc<MetadataService>,
    poster_service: Arc<PosterService>,
    poster_semaphore: Arc<Semaphore>,
    in_progress_urls: Arc<Mutex<HashSet<String>>>,
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
        let needs_poster = records.iter().filter(|r| r.needs_poster_sync()).count();
        let needs_tmdb = records.iter().filter(|r| r.needs_tmdb_sync()).count();

        // Update last_id for next iteration
        if let Some(last) = records.last() {
            last_id = last.id;
        }

        tracing::debug!(
            "Processing chunk {} ({} records: {} posters, {} TMDB IDs)",
            chunk_num,
            chunk_size,
            needs_poster,
            needs_tmdb
        );

        let results: Vec<(Option<bool>, Option<(bool, bool)>)> = stream::iter(records)
            .map(|record| {
                let db = db.clone();
                let metadata = Arc::clone(&metadata_service);
                let poster = Arc::clone(&poster_service);
                let semaphore = Arc::clone(&poster_semaphore);
                let in_progress = Arc::clone(&in_progress_urls);
                async move {
                    process_sync_record(record, db, metadata, poster, semaphore, in_progress).await
                }
            })
            .buffer_unordered(SYNC_CONCURRENCY)
            .collect()
            .await;

        for (poster_result, tmdb_result) in results {
            if let Some(true) = poster_result {
                stats.posters_queued += 1;
            }
            if let Some((succeeded, skipped)) = tmdb_result {
                if succeeded {
                    stats.tmdb_succeeded += 1;
                } else if skipped {
                    stats.tmdb_skipped += 1;
                } else {
                    stats.tmdb_failed += 1;
                }
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
    db: SqlitePool,
    metadata: Arc<MetadataService>,
    poster: Arc<PosterService>,
    semaphore: Arc<Semaphore>,
    in_progress_urls: Arc<Mutex<HashSet<String>>>,
) -> (Option<bool>, Option<(bool, bool)>) {
    let mut poster_result = None;
    let mut tmdb_result = None;
    let mut any_success = false;

    // Queue poster download
    if record.needs_poster_sync() {
        let url = record.poster_url.as_ref().unwrap().clone();
        let queued = spawn_poster_download_task(
            record.id,
            url,
            db.clone(),
            Arc::clone(&poster),
            Arc::clone(&semaphore),
            Arc::clone(&in_progress_urls),
        );
        if queued {
            poster_result = Some(true);
            any_success = true;
        }
    }

    // Sync TMDB ID
    if record.needs_tmdb_sync() {
        let title = record.title_japanese.as_ref().unwrap();
        let result = match metadata.find_tmdb_id(title, record.year).await {
            Ok(Some(tmdb_id)) => {
                match MetadataRepository::update_tmdb_id(&db, record.id, tmdb_id).await {
                    Ok(true) => {
                        any_success = true;
                        (true, false)
                    }
                    Ok(false) => {
                        tracing::warn!("Metadata {} not found when updating TMDB ID", record.id);
                        (false, false)
                    }
                    Err(e) => {
                        tracing::error!(
                            "Failed to update TMDB ID for metadata {}: {}",
                            record.id,
                            e
                        );
                        (false, false)
                    }
                }
            }
            Ok(None) => {
                tracing::debug!("No TMDB match found for metadata {} ({})", record.id, title);
                (false, true) // skipped - no match found
            }
            Err(e) => {
                tracing::warn!(
                    "Failed to search TMDB for metadata {} ({}): {}",
                    record.id,
                    title,
                    e
                );
                (false, false) // failed - API error
            }
        };

        // Update lookup timestamp to prevent repeated lookups (7-day cooldown)
        if let Err(e) = MetadataRepository::update_tmdb_lookup_at(&db, record.id).await {
            tracing::warn!(
                "Failed to update tmdb_lookup_at for metadata {}: {}",
                record.id,
                e
            );
        }

        tmdb_result = Some(result);
    }

    // Log success message
    if any_success {
        tracing::info!("{} metadata refreshed", record.title_chinese);
    }

    (poster_result, tmdb_result)
}
