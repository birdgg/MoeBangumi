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
use crate::repositories::{BangumiRepository, TorrentRepository};
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
/// 5. Generates .nfo metadata files
/// 6. Marks tasks as rename complete
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

        // Group results by bangumi_id: (title, poster_url, episodes)
        let mut grouped: HashMap<i64, (String, Option<String>, Vec<i32>)> = HashMap::new();
        for result in results {
            if result.renamed_episodes.is_empty() {
                continue;
            }
            let entry = grouped.entry(result.bangumi_id).or_insert_with(|| {
                (
                    result.bangumi_title.clone(),
                    result.poster_url.clone(),
                    Vec::new(),
                )
            });
            entry.2.extend(result.renamed_episodes);
        }

        // Process each bangumi: update current_episode and send notification
        for (bangumi_id, (title, poster_url, mut episodes)) in grouped {
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
        let adjusted_episode = bangumi.bangumi.adjust_episode(episode);

        // Generate new filename using pathgen
        let new_filename_base = pathgen::generate_filename(
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

        // Generate NFO file next to the renamed video
        // Calculate absolute directory path by combining task.save_path with file's relative directory
        let nfo_dir = if let Some(parent) = Path::new(&new_path).parent() {
            Path::new(&task.save_path).join(parent)
        } else {
            Path::new(&task.save_path).to_path_buf()
        };
        if let Err(e) = self
            .write_nfo_file(
                &nfo_dir.to_string_lossy(),
                &new_filename_base,
                bangumi,
                adjusted_episode,
                old_path,
            )
            .await
        {
            tracing::warn!("Failed to write NFO file for {}: {}", new_path, e);
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

    /// Write NFO metadata file
    async fn write_nfo_file(
        &self,
        save_path: &str,
        filename_base: &str,
        bangumi: &BangumiWithMetadata,
        episode: i32,
        original_filename: &str,
    ) -> Result<()> {
        let nfo_content = Self::generate_nfo(bangumi, episode, original_filename);

        let nfo_path = Path::new(save_path).join(format!("{}.nfo", filename_base));

        // Create parent directories if needed
        if let Some(parent) = nfo_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        tokio::fs::write(&nfo_path, nfo_content).await?;
        tracing::debug!("Generated NFO: {}", nfo_path.display());

        Ok(())
    }

    /// Generate NFO content in XML format
    fn generate_nfo(
        bangumi: &BangumiWithMetadata,
        episode: i32,
        original_filename: &str,
    ) -> String {
        let mut nfo =
            String::from("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
        nfo.push_str("<episodedetails>\n");

        // Title and show info
        nfo.push_str(&format!("  <title>Episode {}</title>\n", episode));
        nfo.push_str(&format!(
            "  <showtitle>{}</showtitle>\n",
            Self::escape_xml(&bangumi.metadata.title_chinese)
        ));

        // Season and episode
        nfo.push_str(&format!("  <season>{}</season>\n", bangumi.metadata.season));
        nfo.push_str(&format!("  <episode>{}</episode>\n", episode));

        // Year and air date
        nfo.push_str(&format!("  <year>{}</year>\n", bangumi.metadata.year));
        if let Some(ref air_date) = bangumi.metadata.air_date {
            nfo.push_str(&format!("  <aired>{}</aired>\n", air_date));
        }

        // IDs
        if let Some(tmdb_id) = bangumi.metadata.tmdb_id {
            nfo.push_str(&format!("  <tmdbid>{}</tmdbid>\n", tmdb_id));
            nfo.push_str(&format!(
                "  <uniqueid type=\"tmdb\" default=\"true\">{}</uniqueid>\n",
                tmdb_id
            ));
        }
        if let Some(bgmtv_id) = bangumi.metadata.bgmtv_id {
            nfo.push_str(&format!(
                "  <uniqueid type=\"bangumi\">{}</uniqueid>\n",
                bgmtv_id
            ));
        }

        // Original filename for reference
        nfo.push_str(&format!(
            "  <original_filename>{}</original_filename>\n",
            Self::escape_xml(original_filename)
        ));

        nfo.push_str("</episodedetails>\n");
        nfo
    }

    /// Escape XML special characters
    fn escape_xml(s: &str) -> String {
        s.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('"', "&quot;")
            .replace('\'', "&apos;")
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
    fn test_escape_xml() {
        assert_eq!(
            RenameService::escape_xml("Test & <Name>"),
            "Test &amp; &lt;Name&gt;"
        );
        assert_eq!(RenameService::escape_xml("Normal"), "Normal");
    }

    #[test]
    fn test_generate_nfo() {
        use crate::models::{Bangumi, Metadata, Platform, SourceType};

        let metadata = Metadata {
            id: 1,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            mikan_id: None,
            bgmtv_id: Some(12345),
            tmdb_id: Some(67890),
            title_chinese: "测试动画".to_string(),
            title_japanese: None,
            season: 1,
            year: 2024,
            platform: Platform::Tv,
            total_episodes: 12,
            poster_url: None,
            air_date: Some("2024-01-01".to_string()),
            air_week: 1,
        };

        let bangumi = Bangumi {
            id: 1,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            metadata_id: 1,
            episode_offset: 0,
            current_episode: 0,
            auto_complete: true,
            save_path: "/downloads".to_string(),
            source_type: SourceType::WebRip,
        };

        let bangumi_with_metadata = BangumiWithMetadata { bangumi, metadata };

        let nfo = RenameService::generate_nfo(&bangumi_with_metadata, 5, "original.mkv");

        assert!(nfo.contains("<episode>5</episode>"));
        assert!(nfo.contains("<season>1</season>"));
        assert!(nfo.contains("<tmdbid>67890</tmdbid>"));
        assert!(nfo.contains("<showtitle>测试动画</showtitle>"));
    }

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
