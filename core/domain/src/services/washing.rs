//! Torrent washing (洗版) service.
//!
//! This module handles the logic for replacing existing torrents with higher priority ones
//! based on subtitle group, language, and resolution settings.

use downloader::DownloaderError;
use parser::ParseResult;
use sqlx::SqlitePool;
use std::sync::Arc;

use washing::{ComparableTorrent, PriorityCalculator};

use crate::models::{CreateTorrent, Torrent};
use crate::repositories::TorrentRepository;
use crate::services::{AddTaskOptions, DownloaderHandle, SettingsService};

/// Error types for washing operations
#[derive(Debug, thiserror::Error)]
pub enum WashingError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Downloader error: {0}")]
    Downloader(#[from] DownloaderError),
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
    /// Download save path
    pub save_path: &'a str,
    /// Renamed filename for download
    pub rename: &'a str,
}

/// Service for torrent washing (洗版) operations.
///
/// Handles priority-based torrent replacement: when a higher priority torrent
/// (better subtitle group, language, or resolution) becomes available, this service
/// manages the replacement process.
pub struct WashingService {
    db: SqlitePool,
    downloader: Arc<DownloaderHandle>,
    settings: Arc<SettingsService>,
}

impl WashingService {
    /// Create a new washing service
    pub fn new(
        db: SqlitePool,
        downloader: Arc<DownloaderHandle>,
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
    pub fn should_wash(
        &self,
        existing_torrents: &[Torrent],
        new_parse_result: &ParseResult,
    ) -> bool {
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
            subtitle_languages: new_parse_result.subtitle_language.clone(),
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

    /// Execute washing: atomically delete old torrents, create new one, and add download.
    ///
    /// 使用事务确保数据库和下载器操作的一致性：
    /// 1. 开始事务
    /// 2. 删除所有旧 torrent 记录
    /// 3. 创建新 torrent 记录
    /// 4. 从下载器删除旧任务
    /// 5. 添加新下载任务
    /// 6. 全部成功则提交事务，任一失败则回滚
    ///
    /// Returns the info_hashes of deleted torrents.
    pub async fn wash_episode(&self, params: WashParams<'_>) -> Result<Vec<String>, WashingError> {
        tracing::info!(
            "[{}] Washing E{}: replacing {} existing torrent(s) with higher priority resource (group={:?}, lang={:?}, res={:?})",
            params.rss_title,
            params.episode,
            params.existing_torrents.len(),
            params.parse_result.subtitle_group,
            params.parse_result.subtitle_language,
            params.parse_result.resolution,
        );

        // Collect info_hashes for downloader cleanup
        let old_hashes: Vec<String> = params
            .existing_torrents
            .iter()
            .map(|t| t.info_hash.clone())
            .collect();

        // Use transaction to ensure atomicity
        let mut tx = self.db.begin().await?;

        // 1. Delete all existing torrents in transaction
        for existing in params.existing_torrents {
            TorrentRepository::delete_with_executor(&mut *tx, existing.id).await?;
        }

        // 2. Create new torrent in transaction (with parsed metadata)
        let new_torrent = CreateTorrent {
            bangumi_id: params.bangumi_id,
            rss_id: params.rss_id,
            info_hash: params.info_hash.to_string(),
            torrent_url: params.torrent_url.to_string(),
            episode_number: Some(params.episode),
            subtitle_group: params.parse_result.subtitle_group.clone(),
            subtitle_languages: params.parse_result.subtitle_language.clone(),
            resolution: params.parse_result.resolution.clone(),
        };

        TorrentRepository::create_with_executor(&mut *tx, new_torrent).await?;

        // 3. Delete old torrents from downloader
        for hash in &old_hashes {
            self.downloader.delete_task(&[hash], true).await?;
            tracing::info!(
                "[{}] Deleted old torrent from downloader: {}",
                params.rss_title,
                hash
            );
        }

        // 4. Add new download task
        let options = AddTaskOptions::new(params.torrent_url)
            .save_path(params.save_path)
            .rename(params.rename);

        self.downloader.add_task(options).await?;

        // 5. All successful, commit transaction
        tx.commit().await?;

        Ok(old_hashes)
    }
}
