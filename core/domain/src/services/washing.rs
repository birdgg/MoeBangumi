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

    /// Execute washing: replace old torrents with higher priority one.
    ///
    /// 操作顺序设计确保一致性：
    /// 1. 先添加新下载任务（失败时直接返回，无副作用）
    /// 2. 数据库事务：删除旧记录 + 创建新记录
    /// 3. 最后删除旧下载任务（失败只警告，不影响主流程）
    ///
    /// 这个顺序的逻辑：
    /// - 先添加新任务：即使后续失败，多一个下载任务影响很小
    /// - 数据库事务：确保记录一致性
    /// - 最后删除旧任务：即使失败，只是多占用一点空间，不影响功能
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

        // 1. Add new download task first (fail fast with no side effects)
        let options = AddTaskOptions::new(params.torrent_url)
            .save_path(params.save_path)
            .rename(params.rename);
        self.downloader.add_task(options).await?;

        // 2. Database transaction: delete old records + create new record
        let mut tx = self.db.begin().await?;

        for existing in params.existing_torrents {
            TorrentRepository::delete_with_executor(&mut *tx, existing.id).await?;
        }

        let new_torrent = CreateTorrent {
            bangumi_id: Some(params.bangumi_id),
            rss_id: params.rss_id,
            info_hash: params.info_hash.to_string(),
            torrent_url: params.torrent_url.to_string(),
            episode_number: Some(params.episode),
            subtitle_group: params.parse_result.subtitle_group.clone(),
            subtitle_languages: params.parse_result.subtitle_language.clone(),
            resolution: params.parse_result.resolution.clone(),
        };
        TorrentRepository::create_with_executor(&mut *tx, new_torrent).await?;

        tx.commit().await?;

        // 3. Delete old torrents from downloader (non-critical, warn on failure)
        for hash in &old_hashes {
            if let Err(e) = self.downloader.delete_task(&[hash], true).await {
                tracing::warn!(
                    "[{}] Failed to delete old task {}: {} (non-critical)",
                    params.rss_title,
                    hash,
                    e
                );
            } else {
                tracing::info!(
                    "[{}] Deleted old torrent from downloader: {}",
                    params.rss_title,
                    hash
                );
            }
        }

        Ok(old_hashes)
    }
}
