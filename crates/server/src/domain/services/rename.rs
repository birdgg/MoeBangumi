//! File renaming service for media organization
//!
//! This service handles automatic renaming of downloaded media files
//! to be compatible with Plex/Jellyfin naming standards.

use futures::stream::{self, StreamExt};
use parser::Parser;
use sqlx::SqlitePool;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::config::Config;
use crate::models::{BangumiWithMetadata, Torrent};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::{DownloaderHandle, NotificationService, Task, TaskFile};

/// Error type for rename operations
#[derive(Debug, thiserror::Error)]
pub enum RenameError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Downloader error: {0}")]
    Downloader(#[from] downloader::DownloaderError),

    #[error("Torrent not found in database: {0}")]
    TorrentNotFound(String),

    #[error("Bangumi not found for torrent: {0}")]
    BangumiNotFound(i64),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, RenameError>;

/// Result of a rename task processing
#[derive(Debug)]
pub struct RenameTaskResult {
    /// Bangumi ID
    pub bangumi_id: i64,
    /// Bangumi title (Chinese)
    pub bangumi_title: String,
    /// Poster URL (local path like /posters/xxx.jpg)
    pub poster_url: Option<String>,
    /// Total episodes for this bangumi (0 = unknown)
    pub total_episodes: i32,
    /// Successfully renamed episode numbers
    pub renamed_episodes: Vec<i32>,
}

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
    parser: Parser,
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
    ) -> Self {
        Self {
            db,
            downloader,
            notification,
            config,
            parser: Parser::new(),
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
            .map(|(task, torrent, bangumi)| async move {
                match self.process_task(&task, &torrent, &bangumi).await {
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
            let episode_str = Self::format_episode_range(&episodes);
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

    /// Format episode numbers into a readable range string
    ///
    /// Examples:
    /// - [1] -> "01"
    /// - [1, 2, 3] -> "01-03"
    /// - [1, 3, 5] -> "01, 03, 05"
    /// - [1, 2, 3, 5, 6] -> "01-03, 05-06"
    fn format_episode_range(episodes: &[i32]) -> String {
        if episodes.is_empty() {
            return String::new();
        }
        if episodes.len() == 1 {
            return format!("{:02}", episodes[0]);
        }

        let mut ranges: Vec<(i32, i32)> = Vec::new();
        let mut start = episodes[0];
        let mut end = episodes[0];

        for &ep in &episodes[1..] {
            if ep == end + 1 {
                end = ep;
            } else {
                ranges.push((start, end));
                start = ep;
                end = ep;
            }
        }
        ranges.push((start, end));

        ranges
            .into_iter()
            .map(|(s, e)| {
                if s == e {
                    format!("{:02}", s)
                } else {
                    format!("{:02}-{:02}", s, e)
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Get all completed tasks pending rename
    async fn get_pending_tasks(&self) -> Result<Vec<(Task, Torrent, BangumiWithMetadata)>> {
        // Query downloader for completed/seeding tasks with rename tag
        let all_tasks = self.downloader.get_rename_pending_tasks().await?;

        let mut result = Vec::new();

        for task in all_tasks {
            // Find matching torrent in database by info_hash
            if let Some(torrent) = TorrentRepository::get_by_info_hash(&self.db, &task.id).await? {
                // Get associated bangumi with metadata
                if let Some(bangumi_with_metadata) =
                    BangumiRepository::get_with_metadata_by_id(&self.db, torrent.bangumi_id).await?
                {
                    result.push((task, torrent, bangumi_with_metadata));
                } else {
                    tracing::warn!(
                        "Bangumi not found for torrent {} (bangumi_id: {})",
                        task.id,
                        torrent.bangumi_id
                    );
                }
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
    async fn process_task(
        &self,
        task: &Task,
        torrent: &Torrent,
        bangumi: &BangumiWithMetadata,
    ) -> Result<RenameTaskResult> {
        tracing::info!(
            "Processing task: {} ({}) for bangumi: {}",
            task.name,
            task.id,
            bangumi.metadata.title_chinese
        );

        let mut renamed_episodes = Vec::new();

        // Get file list from downloader
        let files = self.downloader.get_task_files(&task.id).await?;

        // Filter to video files only
        let video_files: Vec<_> = files.iter().filter(|f| f.is_video()).collect();

        if video_files.is_empty() {
            tracing::warn!("No video files found in task: {}", task.name);
            // Still remove the tag since there's nothing to rename
            self.finalize_task(&task.id).await?;
            return Ok(RenameTaskResult {
                bangumi_id: bangumi.bangumi.id,
                bangumi_title: bangumi.metadata.title_chinese.clone(),
                poster_url: bangumi.metadata.poster_url.clone(),
                total_episodes: bangumi.metadata.total_episodes,
                renamed_episodes,
            });
        }

        // Process each video file
        for video_file in &video_files {
            // If single video file and torrent has episode_number, use it as fallback
            let episode = if video_files.len() == 1 {
                torrent
                    .episode_number
                    .or_else(|| self.parse_episode_number(&video_file.path))
            } else {
                // Multiple files - always parse from filename
                self.parse_episode_number(&video_file.path)
            };

            if let Some(ep) = episode {
                if self
                    .rename_file(task, video_file, bangumi, ep, &files)
                    .await
                    .is_ok()
                {
                    renamed_episodes.push(ep);
                }
            } else {
                tracing::warn!(
                    "Could not determine episode number for: {}",
                    video_file.path
                );
            }
        }

        // Remove the "rename" tag
        self.finalize_task(&task.id).await?;

        tracing::info!("Successfully renamed task: {}", task.name);
        Ok(RenameTaskResult {
            bangumi_id: bangumi.bangumi.id,
            bangumi_title: bangumi.metadata.title_chinese.clone(),
            poster_url: bangumi.metadata.poster_url.clone(),
            total_episodes: bangumi.metadata.total_episodes,
            renamed_episodes,
        })
    }

    /// Rename a single video file and associated subtitles
    async fn rename_file(
        &self,
        task: &Task,
        file: &TaskFile,
        bangumi: &BangumiWithMetadata,
        episode: i32,
        all_files: &[TaskFile],
    ) -> Result<()> {
        let old_path = &file.path;

        // Get file extension
        let ext = file.extension().unwrap_or("mkv");

        // Apply episode offset to convert RSS episode number to season-relative episode
        let adjusted_episode = bangumi.metadata.adjust_episode(episode);

        // Generate new filename using pathgen
        let new_filename_base = crate::pathgen::generate_filename(
            &bangumi.metadata.title_chinese,
            bangumi.metadata.season,
            adjusted_episode,
            Some(bangumi.metadata.platform.as_str()),
        );

        let new_filename = format!("{}.{}", new_filename_base, ext);

        // Preserve directory structure
        let new_path = Self::join_with_parent(old_path, &new_filename);

        // Rename associated subtitle files FIRST (before video rename to avoid path cache issues)
        self.rename_subtitles(task, old_path, &new_filename_base, all_files)
            .await?;

        // Now rename the video file
        if old_path == &new_path {
            tracing::debug!("File already has correct name: {}", old_path);
        } else {
            tracing::info!("Renaming: {} -> {}", old_path, new_path);
            self.downloader
                .rename_file(&task.id, old_path, &new_path)
                .await?;
        }

        Ok(())
    }

    /// Rename subtitle files that match the video file
    async fn rename_subtitles(
        &self,
        task: &Task,
        old_video_path: &str,
        new_basename: &str,
        all_files: &[TaskFile],
    ) -> Result<()> {
        // Get the basename of the old video (without extension)
        let old_basename = Path::new(old_video_path)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("");

        if old_basename.is_empty() {
            return Ok(());
        }

        // Find subtitle files with matching basename
        let subtitle_exts = ["ass", "srt", "ssa", "sub", "vtt"];

        for file in all_files {
            let file_path = Path::new(&file.path);

            // Check if it's a subtitle file
            let is_subtitle = file_path
                .extension()
                .and_then(|e| e.to_str())
                .map(|e| subtitle_exts.contains(&e.to_lowercase().as_str()))
                .unwrap_or(false);

            if !is_subtitle {
                continue;
            }

            // Check if it matches our video file
            // Subtitle files might be named like:
            // - video.ass
            // - video.zh-CN.ass
            // - video.简体中文.ass
            let file_name = file_path.file_name().and_then(|s| s.to_str()).unwrap_or("");

            if !file_name.starts_with(old_basename) {
                continue;
            }

            // Extract the suffix (language tag + extension)
            let suffix = &file_name[old_basename.len()..];
            let new_subtitle_name = format!("{}{}", new_basename, suffix);

            // Build the new path preserving directory
            let new_subtitle_path = Self::join_with_parent(&file.path, &new_subtitle_name);

            if file.path != new_subtitle_path {
                tracing::info!("Renaming subtitle: {} -> {}", file.path, new_subtitle_path);
                self.downloader
                    .rename_file(&task.id, &file.path, &new_subtitle_path)
                    .await?;
            }
        }

        Ok(())
    }

    /// Parse episode number from filename using the parser
    fn parse_episode_number(&self, filename: &str) -> Option<i32> {
        // Extract just the filename part
        let name = Path::new(filename)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(filename);

        match self.parser.parse(name) {
            Ok(result) => result.episode,
            Err(_) => None,
        }
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

    /// Join filename with parent directory, preserving directory structure
    fn join_with_parent(original_path: &str, new_filename: &str) -> String {
        let path = Path::new(original_path);
        if let Some(parent) = path.parent() {
            if parent.as_os_str().is_empty() {
                new_filename.to_string()
            } else {
                parent.join(new_filename).to_string_lossy().to_string()
            }
        } else {
            new_filename.to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_episode_range() {
        // Empty
        assert_eq!(RenameService::format_episode_range(&[]), "");

        // Single episode
        assert_eq!(RenameService::format_episode_range(&[1]), "01");
        assert_eq!(RenameService::format_episode_range(&[12]), "12");

        // Consecutive range
        assert_eq!(RenameService::format_episode_range(&[1, 2, 3]), "01-03");
        assert_eq!(
            RenameService::format_episode_range(&[8, 9, 10, 11, 12]),
            "08-12"
        );

        // Non-consecutive
        assert_eq!(
            RenameService::format_episode_range(&[1, 3, 5]),
            "01, 03, 05"
        );

        // Mixed ranges
        assert_eq!(
            RenameService::format_episode_range(&[1, 2, 3, 5, 6]),
            "01-03, 05-06"
        );
        assert_eq!(
            RenameService::format_episode_range(&[1, 2, 5, 6, 7, 10]),
            "01-02, 05-07, 10"
        );
    }
}
