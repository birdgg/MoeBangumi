use async_trait::async_trait;
use futures::{stream, StreamExt};
use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::repositories::{MetadataRepository, MetadataToSync};
use crate::services::{MetadataService, PosterService};

/// Concurrency limit for metadata sync operations
const METADATA_SYNC_CONCURRENCY: usize = 5;

/// Statistics for the sync operation
#[derive(Default)]
struct SyncStats {
    poster_succeeded: usize,
    poster_failed: usize,
    tmdb_succeeded: usize,
    tmdb_failed: usize,
    tmdb_skipped: usize,
}

/// Metadata sync job that downloads remote poster images and fills missing TMDB IDs.
///
/// This job runs every 24 hours and processes metadata records that need syncing:
/// - Downloads remote poster URLs to local storage
/// - Searches TMDB using Japanese title for records without TMDB ID
pub struct MetadataSyncJob {
    db: SqlitePool,
    poster: Arc<PosterService>,
    metadata: Arc<MetadataService>,
}

impl MetadataSyncJob {
    /// Creates a new metadata sync job.
    pub fn new(
        db: SqlitePool,
        poster: Arc<PosterService>,
        metadata: Arc<MetadataService>,
    ) -> Self {
        Self { db, poster, metadata }
    }

    /// Process a single metadata record for syncing.
    async fn process_metadata(
        record: MetadataToSync,
        db: SqlitePool,
        poster: Arc<PosterService>,
        metadata: Arc<MetadataService>,
    ) -> (Option<bool>, Option<(bool, bool)>) {
        let mut poster_result = None;
        let mut tmdb_result = None;

        // Sync poster if needed
        if record.needs_poster_sync() {
            let poster_url = record.poster_url.as_ref().unwrap();
            poster_result = Some(match poster.download_from_url(poster_url).await {
                Ok(local_path) => {
                    match MetadataRepository::update_poster_url(&db, record.id, &local_path).await {
                        Ok(true) => {
                            tracing::info!(
                                "Synced poster for metadata {}: {} -> {}",
                                record.id,
                                poster_url,
                                local_path
                            );
                            true
                        }
                        Ok(false) => {
                            tracing::warn!(
                                "Metadata {} not found when updating poster",
                                record.id
                            );
                            false
                        }
                        Err(e) => {
                            tracing::error!(
                                "Failed to update poster URL for metadata {}: {}",
                                record.id,
                                e
                            );
                            false
                        }
                    }
                }
                Err(e) => {
                    tracing::warn!(
                        "Failed to download poster for metadata {}: {}",
                        record.id,
                        e
                    );
                    false
                }
            });
        }

        // Sync TMDB ID if needed
        if record.needs_tmdb_sync() {
            let title = record.title_japanese.as_ref().unwrap();
            tmdb_result = Some(match metadata.find_tmdb_id(title).await {
                Ok(Some(tmdb_id)) => {
                    match MetadataRepository::update_tmdb_id(&db, record.id, tmdb_id).await {
                        Ok(true) => {
                            tracing::info!(
                                "Found TMDB ID {} for metadata {} ({})",
                                tmdb_id,
                                record.id,
                                title
                            );
                            (true, false) // succeeded
                        }
                        Ok(false) => {
                            tracing::warn!(
                                "Metadata {} not found when updating TMDB ID",
                                record.id
                            );
                            (false, false) // failed
                        }
                        Err(e) => {
                            tracing::error!(
                                "Failed to update TMDB ID for metadata {}: {}",
                                record.id,
                                e
                            );
                            (false, false) // failed
                        }
                    }
                }
                Ok(None) => {
                    tracing::debug!(
                        "No TMDB match found for metadata {} ({})",
                        record.id,
                        title
                    );
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
            });
        }

        (poster_result, tmdb_result)
    }
}

#[async_trait]
impl SchedulerJob for MetadataSyncJob {
    fn name(&self) -> &'static str {
        "MetadataSync"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(86400) // Every 24 hours
    }

    async fn execute(&self) -> JobResult {
        tracing::info!("Running metadata sync job");

        let records = match MetadataRepository::get_metadata_to_sync(&self.db).await {
            Ok(records) => records,
            Err(e) => {
                tracing::error!("Failed to get metadata to sync: {}", e);
                return Ok(());
            }
        };

        if records.is_empty() {
            tracing::debug!("No metadata records need syncing");
            return Ok(());
        }

        let needs_poster = records.iter().filter(|r| r.needs_poster_sync()).count();
        let needs_tmdb = records.iter().filter(|r| r.needs_tmdb_sync()).count();
        tracing::info!(
            "Found {} metadata records to sync ({} posters, {} TMDB IDs)",
            records.len(),
            needs_poster,
            needs_tmdb
        );

        let results: Vec<(Option<bool>, Option<(bool, bool)>)> = stream::iter(records)
            .map(|record| {
                let db = self.db.clone();
                let poster = Arc::clone(&self.poster);
                let metadata = Arc::clone(&self.metadata);
                async move { Self::process_metadata(record, db, poster, metadata).await }
            })
            .buffer_unordered(METADATA_SYNC_CONCURRENCY)
            .collect()
            .await;

        let mut stats = SyncStats::default();
        for (poster_result, tmdb_result) in results {
            if let Some(succeeded) = poster_result {
                if succeeded {
                    stats.poster_succeeded += 1;
                } else {
                    stats.poster_failed += 1;
                }
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

        tracing::info!(
            "Metadata sync completed: posters ({} succeeded, {} failed), TMDB IDs ({} succeeded, {} failed, {} skipped)",
            stats.poster_succeeded,
            stats.poster_failed,
            stats.tmdb_succeeded,
            stats.tmdb_failed,
            stats.tmdb_skipped
        );

        Ok(())
    }
}
