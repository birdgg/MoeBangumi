//! Torrent washing (洗版) service.
//!
//! This module handles the logic for replacing existing torrents with higher priority ones
//! based on subtitle group, language, and resolution settings.

use parser::ParseResult;
use sqlx::SqlitePool;
use std::sync::Arc;

use washing::{ComparableTorrent, PriorityCalculator};

use crate::models::{CreateTorrent, Torrent};
use crate::repositories::TorrentRepository;
use crate::services::{DownloaderService, SettingsService};

/// Error types for washing operations
#[derive(Debug, thiserror::Error)]
pub enum WashingError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
}

/// Parameters for washing an episode
pub struct WashParams<'a> {
    pub bangumi_id: i64,
    pub rss_id: Option<i64>,
    pub rss_title: &'a str,
    pub existing_torrents: &'a [Torrent],
    pub info_hash: &'a str,
    pub torrent_url: &'a str,
    pub episode: i32,
    pub parse_result: &'a ParseResult,
}

/// Service for torrent washing (洗版) operations.
///
/// Handles priority-based torrent replacement: when a higher priority torrent
/// (better subtitle group, language, or resolution) becomes available, this service
/// manages the replacement process.
pub struct WashingService {
    db: SqlitePool,
    downloader: Arc<DownloaderService>,
    settings: Arc<SettingsService>,
}

impl WashingService {
    /// Create a new washing service
    pub fn new(
        db: SqlitePool,
        downloader: Arc<DownloaderService>,
        settings: Arc<SettingsService>,
    ) -> Self {
        Self {
            db,
            downloader,
            settings,
        }
    }

    /// Determine if we should replace existing episode torrents based on priority.
    ///
    /// Returns true only if the new torrent has higher priority than
    /// the best existing torrent (comparing subtitle group, language, resolution).
    pub fn should_wash(&self, existing_torrents: &[Torrent], new_parse_result: &ParseResult) -> bool {
        if existing_torrents.is_empty() {
            return false; // Nothing to wash, caller should use normal add
        }

        // Build priority calculator from current settings
        let settings = self.settings.get();
        let priority_config = settings.priority.to_config();
        let calculator = PriorityCalculator::new(priority_config);

        // New torrent's comparable info
        let new_comparable = ComparableTorrent {
            subtitle_group: new_parse_result.subtitle_group.clone(),
            subtitle_language: if new_parse_result.sub_type.is_empty() {
                None
            } else {
                Some(new_parse_result.sub_type.join(" "))
            },
            resolution: new_parse_result.resolution.clone(),
        };

        // Convert existing torrents to comparable form
        let existing_comparables: Vec<ComparableTorrent> = existing_torrents
            .iter()
            .map(|t| t.to_comparable())
            .collect();

        // Find the best existing torrent
        let best_existing = match calculator.find_best(&existing_comparables) {
            Some(best) => best,
            None => return true, // No valid existing torrents, should wash
        };

        // Compare: new must be strictly higher priority to trigger washing
        calculator.is_higher_priority(&new_comparable, best_existing)
    }

    /// Execute washing: atomically delete old torrents and create new one.
    ///
    /// This method:
    /// 1. Deletes all existing torrents for the episode from database (in a transaction)
    /// 2. Creates the new torrent record
    /// 3. Removes old torrents from downloader (best-effort)
    ///
    /// Returns the info_hashes of deleted torrents for caller to handle download cleanup.
    pub async fn wash_episode(&self, params: WashParams<'_>) -> Result<Vec<String>, WashingError> {
        tracing::info!(
            "[{}] Washing E{}: replacing {} existing torrent(s) with higher priority resource (group={:?}, lang={:?}, res={:?})",
            params.rss_title,
            params.episode,
            params.existing_torrents.len(),
            params.parse_result.subtitle_group,
            params.parse_result.sub_type,
            params.parse_result.resolution,
        );

        // Collect info_hashes for downloader cleanup (done after transaction)
        let old_hashes: Vec<String> = params
            .existing_torrents
            .iter()
            .map(|t| t.info_hash.clone())
            .collect();

        // Use transaction to ensure atomicity: delete old + create new
        let mut tx = self.db.begin().await?;

        // Delete all existing torrents in transaction
        for existing in params.existing_torrents {
            if let Err(e) = TorrentRepository::delete_with_executor(&mut *tx, existing.id).await {
                tracing::error!(
                    "[{}] Failed to delete old torrent from database: {}",
                    params.rss_title,
                    e
                );
                return Err(WashingError::Database(e));
            }
        }

        // Create new torrent in transaction (with parsed metadata)
        let new_torrent = CreateTorrent {
            bangumi_id: params.bangumi_id,
            rss_id: params.rss_id,
            info_hash: params.info_hash.to_string(),
            torrent_url: params.torrent_url.to_string(),
            episode_number: Some(params.episode),
            subtitle_group: params.parse_result.subtitle_group.clone(),
            subtitle_language: if params.parse_result.sub_type.is_empty() {
                None
            } else {
                Some(params.parse_result.sub_type.join(" "))
            },
            resolution: params.parse_result.resolution.clone(),
        };

        if let Err(e) = TorrentRepository::create_with_executor(&mut *tx, new_torrent).await {
            tracing::error!("[{}] Failed to create new torrent: {}", params.rss_title, e);
            return Err(WashingError::Database(e));
        }

        // Commit transaction
        tx.commit().await?;

        // Transaction committed successfully, now handle downloader operations (best-effort)
        for hash in &old_hashes {
            self.delete_from_downloader(hash, params.rss_title).await;
        }

        Ok(old_hashes)
    }

    /// Delete a torrent from the downloader by info_hash.
    ///
    /// Used during washing when replacing torrents. Errors are logged but not
    /// propagated since:
    /// - Task might not exist (already deleted/completed manually)
    /// - Downloader connection issues shouldn't block database cleanup
    async fn delete_from_downloader(&self, info_hash: &str, rss_title: &str) {
        match self.downloader.delete_task(&[info_hash], true).await {
            Ok(_) => {
                tracing::info!(
                    "[{}] Deleted old torrent from downloader: {}",
                    rss_title,
                    info_hash
                );
            }
            Err(e) => {
                tracing::warn!(
                    "[{}] Could not delete torrent from downloader (hash={}): {}",
                    rss_title,
                    info_hash,
                    e
                );
            }
        }
    }
}
