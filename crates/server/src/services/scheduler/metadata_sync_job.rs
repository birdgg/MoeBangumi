use async_trait::async_trait;
use futures::{stream, StreamExt};
use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::repositories::MetadataRepository;
use crate::services::{MetadataService, PosterService};

/// Concurrency limit for metadata sync operations
const METADATA_SYNC_CONCURRENCY: usize = 5;

/// Statistics for a sync operation
#[derive(Default)]
struct SyncStats {
    succeeded: usize,
    failed: usize,
    skipped: usize,
}

/// Metadata sync job that downloads remote poster images and fills missing TMDB IDs.
///
/// This job runs every 24 hours and:
/// 1. Scans the metadata table for records with remote poster URLs and downloads them
/// 2. Scans the metadata table for records without TMDB ID and searches using Japanese title
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

    /// Sync remote poster URLs to local storage.
    async fn sync_posters(&self) -> SyncStats {
        let remote_posters = match MetadataRepository::get_remote_poster_metadata(&self.db).await {
            Ok(posters) => posters,
            Err(e) => {
                tracing::error!("Failed to get remote poster metadata: {}", e);
                return SyncStats::default();
            }
        };

        if remote_posters.is_empty() {
            tracing::debug!("No remote poster URLs found");
            return SyncStats::default();
        }

        tracing::info!(
            "Found {} metadata records with remote poster URLs",
            remote_posters.len()
        );

        let results: Vec<(bool, bool)> = stream::iter(remote_posters)
            .map(|(metadata_id, poster_url)| {
                let poster = Arc::clone(&self.poster);
                let db = self.db.clone();
                async move {
                    match poster.download_from_url(&poster_url).await {
                        Ok(local_path) => {
                            match MetadataRepository::update_poster_url(&db, metadata_id, &local_path)
                                .await
                            {
                                Ok(true) => {
                                    tracing::info!(
                                        "Synced poster for metadata {}: {} -> {}",
                                        metadata_id,
                                        poster_url,
                                        local_path
                                    );
                                    (true, false) // succeeded
                                }
                                Ok(false) => {
                                    tracing::warn!(
                                        "Metadata {} not found when updating poster",
                                        metadata_id
                                    );
                                    (false, true) // failed
                                }
                                Err(e) => {
                                    tracing::error!(
                                        "Failed to update poster URL for metadata {}: {}",
                                        metadata_id,
                                        e
                                    );
                                    (false, true) // failed
                                }
                            }
                        }
                        Err(e) => {
                            tracing::warn!(
                                "Failed to download poster for metadata {}: {}",
                                metadata_id,
                                e
                            );
                            (false, true) // failed
                        }
                    }
                }
            })
            .buffer_unordered(METADATA_SYNC_CONCURRENCY)
            .collect()
            .await;

        let mut stats = SyncStats::default();
        for (succeeded, failed) in results {
            if succeeded {
                stats.succeeded += 1;
            } else if failed {
                stats.failed += 1;
            }
        }

        tracing::info!(
            "Poster sync completed: {} succeeded, {} failed",
            stats.succeeded,
            stats.failed
        );

        stats
    }

    /// Sync TMDB IDs for metadata records without one.
    async fn sync_tmdb_ids(&self) -> SyncStats {
        let missing_tmdb = match MetadataRepository::get_metadata_without_tmdb_id(&self.db).await {
            Ok(records) => records,
            Err(e) => {
                tracing::error!("Failed to get metadata without TMDB ID: {}", e);
                return SyncStats::default();
            }
        };

        if missing_tmdb.is_empty() {
            tracing::debug!("No metadata records missing TMDB ID");
            return SyncStats::default();
        }

        tracing::info!(
            "Found {} metadata records missing TMDB ID",
            missing_tmdb.len()
        );

        let results: Vec<(bool, bool, bool)> = stream::iter(missing_tmdb)
            .map(|(metadata_id, title_japanese)| {
                let metadata = Arc::clone(&self.metadata);
                let db = self.db.clone();
                async move {
                    match metadata.find_tmdb_id(&title_japanese).await {
                        Ok(Some(tmdb_id)) => {
                            match MetadataRepository::update_tmdb_id(&db, metadata_id, tmdb_id).await
                            {
                                Ok(true) => {
                                    tracing::info!(
                                        "Found TMDB ID {} for metadata {} ({})",
                                        tmdb_id,
                                        metadata_id,
                                        title_japanese
                                    );
                                    (true, false, false) // succeeded
                                }
                                Ok(false) => {
                                    tracing::warn!(
                                        "Metadata {} not found when updating TMDB ID",
                                        metadata_id
                                    );
                                    (false, true, false) // failed
                                }
                                Err(e) => {
                                    tracing::error!(
                                        "Failed to update TMDB ID for metadata {}: {}",
                                        metadata_id,
                                        e
                                    );
                                    (false, true, false) // failed
                                }
                            }
                        }
                        Ok(None) => {
                            tracing::debug!(
                                "No TMDB match found for metadata {} ({})",
                                metadata_id,
                                title_japanese
                            );
                            (false, false, true) // skipped - no match found
                        }
                        Err(e) => {
                            tracing::warn!(
                                "Failed to search TMDB for metadata {} ({}): {}",
                                metadata_id,
                                title_japanese,
                                e
                            );
                            (false, true, false) // failed - API error
                        }
                    }
                }
            })
            .buffer_unordered(METADATA_SYNC_CONCURRENCY)
            .collect()
            .await;

        let mut stats = SyncStats::default();
        for (succeeded, failed, skipped) in results {
            if succeeded {
                stats.succeeded += 1;
            } else if failed {
                stats.failed += 1;
            } else if skipped {
                stats.skipped += 1;
            }
        }

        tracing::info!(
            "TMDB ID sync completed: {} succeeded, {} failed, {} skipped",
            stats.succeeded,
            stats.failed,
            stats.skipped
        );

        stats
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

        // Run poster sync (errors are handled internally)
        self.sync_posters().await;

        // Run TMDB ID sync (errors are handled internally)
        self.sync_tmdb_ids().await;

        tracing::info!("Metadata sync job completed");

        Ok(())
    }
}
