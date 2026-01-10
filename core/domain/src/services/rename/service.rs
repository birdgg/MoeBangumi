//! Main rename service implementation.
//!
//! This service handles automatic renaming of downloaded media files
//! to be compatible with Plex/Jellyfin naming standards.

use futures::stream::{self, StreamExt};
use sqlx::SqlitePool;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use crate::config::Config;
use crate::models::{BangumiWithSeries, SourceType, Torrent};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::{DownloaderHandle, NotificationService, SettingsService, Task, TaskFile};

use super::bdrip::BDRipProcessor;
use super::paths::format_episode_range;
use super::standard::StandardProcessor;
use super::{RenameError, RenameTaskResult, Result};

/// Service for renaming downloaded media files to Plex/Jellyfin compatible names.
///
/// The service:
/// 1. Queries completed tasks pending rename from downloader
/// 2. Matches them with Torrent records in the database
/// 3. Gets Bangumi metadata for proper naming
/// 4. Renames video files and associated subtitles
/// 5. Marks tasks as rename complete
pub struct RenameService {
    db: SqlitePool,
    downloader: Arc<DownloaderHandle>,
    notification: Arc<NotificationService>,
    config: Arc<Config>,
    settings: Arc<SettingsService>,
    bdrip_processor: BDRipProcessor,
    standard_processor: StandardProcessor,
    /// Maximum number of concurrent rename tasks
    concurrency: usize,
}

impl RenameService {
    /// Create a new RenameService with default concurrency of 4
    pub fn new(
        db: SqlitePool,
        downloader: Arc<DownloaderHandle>,
        notification: Arc<NotificationService>,
        config: Arc<Config>,
        settings: Arc<SettingsService>,
    ) -> Self {
        Self {
            db,
            bdrip_processor: BDRipProcessor::new(Arc::clone(&downloader)),
            standard_processor: StandardProcessor::new(Arc::clone(&downloader)),
            downloader,
            notification,
            config,
            settings,
            concurrency: 4,
        }
    }

    /// Set the maximum number of concurrent rename tasks
    pub fn with_concurrency(mut self, concurrency: usize) -> Self {
        self.concurrency = concurrency;
        self
    }

    /// Process all pending rename tasks
    ///
    /// This is the main entry point called by the scheduler.
    pub async fn process_all(&self) -> Result<()> {
        let pending = self.get_pending_tasks().await?;

        if pending.is_empty() {
            tracing::debug!("No tasks pending rename");
            return Ok(());
        }

        tracing::info!("Found {} tasks to rename", pending.len());

        // Collect results from all tasks
        let results: Vec<RenameTaskResult> = stream::iter(pending)
            .map(|(task, torrent, all_bangumi)| async move {
                match self.process_task(&task, &torrent, &all_bangumi).await {
                    Ok(result) => Some(result),
                    Err(e) => {
                        tracing::error!(
                            "Failed to rename task '{}' ({}): {}",
                            task.name,
                            task.id,
                            e
                        );
                        None
                    }
                }
            })
            .buffer_unordered(self.concurrency)
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect();

        // Group results by bangumi_id: (title, poster_url, total_episodes, episodes)
        let mut grouped: HashMap<i64, (String, Option<String>, i32, Vec<i32>)> = HashMap::new();
        for result in results {
            if result.renamed_episodes.is_empty() {
                continue;
            }
            let entry = grouped.entry(result.bangumi_id).or_insert_with(|| {
                (
                    result.bangumi_title.clone(),
                    result.poster_url.clone(),
                    result.total_episodes,
                    Vec::new(),
                )
            });
            entry.3.extend(result.renamed_episodes);
        }

        // Process each bangumi: update current_episode and send notification
        for (bangumi_id, (title, poster_url, total_episodes, mut episodes)) in grouped {
            if episodes.is_empty() {
                continue;
            }

            // Sort and deduplicate first, then get max from sorted list
            episodes.sort_unstable();
            episodes.dedup();

            let Some(&max_episode) = episodes.last() else {
                continue;
            };

            // Update current_episode (only if greater than current value)
            if let Err(e) = BangumiRepository::update_current_episode_if_greater(
                &self.db,
                bangumi_id,
                max_episode,
            )
            .await
            {
                tracing::warn!(
                    "Failed to update current_episode for bangumi {}: {}",
                    bangumi_id,
                    e
                );
            }

            // Auto-disable RSS when all episodes are downloaded
            // Only check if total_episodes is known (> 0)
            if total_episodes > 0 && max_episode >= total_episodes {
                match RssRepository::disable_by_bangumi_id(&self.db, bangumi_id).await {
                    Ok(count) if count > 0 => {
                        tracing::info!(
                            "Auto-disabled {} RSS subscription(s) for completed bangumi '{}' (episode {}/{})",
                            count,
                            title,
                            max_episode,
                            total_episodes
                        );
                    }
                    Err(e) => {
                        tracing::warn!(
                            "Failed to auto-disable RSS for bangumi {}: {}",
                            bangumi_id,
                            e
                        );
                    }
                    _ => {}
                }
            }

            // Send notification with poster if available
            let episode_str = format_episode_range(&episodes);
            let notification_title = format!("{} 第{}话", title, episode_str);
            let notification_content = "已更新";

            // Send notification (fire-and-forget)
            if let Some(ref poster_path) = poster_url {
                self.send_notification_with_poster(
                    &notification_title,
                    notification_content,
                    poster_path,
                )
                .await;
            } else {
                self.notification
                    .notify_download(&notification_title, notification_content);
            }
        }

        Ok(())
    }

    /// Get all completed tasks pending rename
    /// Returns tasks with all associated bangumi (sorted by season for multi-season BDRip)
    async fn get_pending_tasks(&self) -> Result<Vec<(Task, Torrent, Vec<BangumiWithSeries>)>> {
        // Query downloader for completed/seeding tasks with rename tag
        let all_tasks = self.downloader.get_rename_pending_tasks().await?;

        let mut result = Vec::new();

        for task in all_tasks {
            // Find matching torrent in database by info_hash
            if let Some(torrent) = TorrentRepository::get_by_info_hash(&self.db, &task.id).await? {
                // Get all associated bangumi IDs (for multi-season BDRip support)
                let bangumi_ids = TorrentRepository::get_bangumi_ids(&self.db, torrent.id).await?;
                if bangumi_ids.is_empty() {
                    tracing::warn!(
                        "No bangumi associated with torrent {} ({})",
                        task.id,
                        torrent.id
                    );
                    continue;
                }

                // Fetch all bangumi with series (sorted by season)
                let all_bangumi =
                    BangumiRepository::get_with_series_by_ids(&self.db, &bangumi_ids).await?;

                if all_bangumi.is_empty() {
                    tracing::warn!(
                        "No bangumi found for torrent {} (bangumi_ids: {:?})",
                        task.id,
                        bangumi_ids
                    );
                    continue;
                }

                result.push((task, torrent, all_bangumi));
            } else {
                // TODO: support it after implement the metadata searcher
                tracing::debug!(
                    "Task '{}' ({}) not found in database, skipping",
                    task.name,
                    task.id
                );
            }
        }

        Ok(result)
    }

    /// Process a single task
    ///
    /// Returns the result containing bangumi info and successfully renamed episodes.
    /// For multi-season BDRip, all_bangumi contains all associated seasons.
    async fn process_task(
        &self,
        task: &Task,
        torrent: &Torrent,
        all_bangumi: &[BangumiWithSeries],
    ) -> Result<RenameTaskResult> {
        // Use first bangumi for logging and result (primary season)
        let primary_bangumi = all_bangumi
            .first()
            .ok_or_else(|| RenameError::BangumiNotFound(torrent.id))?;

        tracing::info!(
            "Processing task: {} ({}) for bangumi: {} ({} season(s))",
            task.name,
            task.id,
            primary_bangumi.bangumi.title_chinese,
            all_bangumi.len()
        );

        // Check if task is in temporary directory and move to final location first
        use crate::utils::is_temp_download_path;
        if is_temp_download_path(&task.save_path) {
            // Get save path from settings
            let base_save_path = &self.settings.get().downloader.save_path;

            // Generate final path dynamically (use primary bangumi for temp path)
            let final_path = pathgen::generate_directory(
                base_save_path,
                &primary_bangumi.series.title_chinese,
                primary_bangumi.bangumi.year,
                primary_bangumi.bangumi.season,
                primary_bangumi.series.tmdb_id,
                Some(primary_bangumi.bangumi.platform.as_str()),
            )
            .unwrap_or_else(|_| {
                format!(
                    "{}/{} ({})/Season {:02}",
                    base_save_path,
                    primary_bangumi.series.title_chinese,
                    primary_bangumi.bangumi.year,
                    primary_bangumi.bangumi.season
                )
            });

            tracing::info!(
                "Moving task from temporary location {} to final location {}",
                task.save_path,
                final_path
            );

            match self.downloader.set_location(&task.id, &final_path).await {
                Ok(()) => {
                    tracing::info!("Successfully moved task {} to {}", task.id, final_path);
                }
                Err(e) => {
                    tracing::error!(
                        "Failed to move task {} to final location: {}. Will retry on next rename cycle.",
                        task.id,
                        e
                    );
                    // Return error to skip this task - it will be retried next cycle
                    // since the rename tag hasn't been removed
                    return Err(RenameError::Downloader(e));
                }
            }
        }

        // Get file list from downloader
        let files = self.downloader.get_task_files(&task.id).await?;

        // Filter to video files only
        let video_files: Vec<_> = files.iter().filter(|f| f.is_video()).collect();

        if video_files.is_empty() {
            tracing::warn!("No video files found in task: {}", task.name);
            // Still remove the tag since there's nothing to rename
            self.finalize_task(&task.id).await?;
            return Ok(RenameTaskResult {
                bangumi_id: primary_bangumi.bangumi.id,
                bangumi_title: primary_bangumi.bangumi.title_chinese.clone(),
                poster_url: primary_bangumi.bangumi.poster_url.clone(),
                total_episodes: primary_bangumi.bangumi.total_episodes,
                renamed_episodes: Vec::new(),
            });
        }

        // Check if this is a BDRip task
        let is_bdrip = self.is_bdrip(task, primary_bangumi, &files);

        let renamed_episodes = if is_bdrip {
            tracing::info!("Detected BDRip structure for task: {}", task.name);
            self.bdrip_processor
                .process(task, &video_files, all_bangumi, &files)
                .await?
        } else {
            self.standard_processor
                .process(task, &video_files, torrent, primary_bangumi, &files)
                .await?
        };

        // Remove the "rename" tag
        self.finalize_task(&task.id).await?;

        tracing::info!("Successfully renamed task: {}", task.name);
        Ok(RenameTaskResult {
            bangumi_id: primary_bangumi.bangumi.id,
            bangumi_title: primary_bangumi.bangumi.title_chinese.clone(),
            poster_url: primary_bangumi.bangumi.poster_url.clone(),
            total_episodes: primary_bangumi.bangumi.total_episodes,
            renamed_episodes,
        })
    }

    /// Check if a task is a BDRip (three-way detection)
    ///
    /// 1. User marked source_type as BDRip
    /// 2. Torrent title contains "bdrip" (case insensitive)
    /// 3. Directory structure contains BDRip indicators (SPs, CDs, Scans)
    fn is_bdrip(&self, task: &Task, bangumi: &BangumiWithSeries, files: &[TaskFile]) -> bool {
        // 1. User marked as BDRip
        if bangumi.bangumi.source_type == SourceType::BDRip {
            return true;
        }

        // 2. Torrent title contains "bdrip"
        if task.name.to_lowercase().contains("bdrip") {
            return true;
        }

        // 3. Directory structure detection
        self.is_bdrip_structure(files)
    }

    /// Check if file structure indicates BDRip content
    ///
    /// Detects BDRip by looking for characteristic directories:
    /// - SPs/SP/Specials: Special episodes
    /// - CDs/CD: Music/OST
    /// - Scans/Scan: Booklet scans
    fn is_bdrip_structure(&self, files: &[TaskFile]) -> bool {
        // Use case-insensitive matching for directory names
        let markers = [
            "/sps/",
            "/sp/",
            "/specials/",
            "/cds/",
            "/cd/",
            "/scans/",
            "/scan/",
        ];

        files.iter().any(|f| {
            let path_lower = f.path.to_lowercase();
            markers.iter().any(|m| path_lower.contains(m))
        })
    }

    /// Mark task rename as complete
    async fn finalize_task(&self, task_id: &str) -> Result<()> {
        self.downloader.complete_rename(task_id).await?;
        Ok(())
    }

    /// Send notification with poster image
    ///
    /// Reads the poster file from disk and sends it with the notification.
    /// Falls back to text-only notification if the poster cannot be read.
    /// Uses poster_path as cache key for Telegram file_id caching.
    async fn send_notification_with_poster(&self, title: &str, content: &str, poster_path: &str) {
        // poster_path is like "/posters/xxx.jpg", need to convert to absolute path
        let poster_file = if poster_path.starts_with("/posters/") {
            self.config.posters_path().join(&poster_path[9..]) // Skip "/posters/"
        } else {
            PathBuf::from(poster_path)
        };

        // Try to read the poster file
        match tokio::fs::read(&poster_file).await {
            Ok(photo_data) => {
                // Use poster_path as cache_key for Telegram file_id caching
                self.notification.notify_download_with_photo(
                    title,
                    content,
                    photo_data,
                    Some(poster_path.to_string()),
                );
            }
            Err(e) => {
                tracing::debug!(
                    "Could not read poster file {:?}: {}, falling back to text",
                    poster_file,
                    e
                );
                self.notification.notify_download(title, content);
            }
        }
    }
}
