//! File renaming service for media organization
//!
//! This service handles automatic renaming of downloaded media files
//! to be compatible with Plex/Jellyfin naming standards.

use futures::stream::{self, StreamExt};
use parser::Parser;
use sqlx::SqlitePool;
use std::path::Path;
use std::sync::Arc;

use crate::models::{Bangumi, Torrent};
use crate::repositories::{BangumiRepository, TorrentRepository};
use crate::services::{DownloaderService, Task, TaskFile, TaskFilter, TaskStatus};

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

/// Service for renaming downloaded media files to Plex/Jellyfin compatible names.
///
/// The service:
/// 1. Queries tasks with "rename" tag that are completed
/// 2. Matches them with Torrent records in the database
/// 3. Gets Bangumi metadata for proper naming
/// 4. Renames video files and associated subtitles
/// 5. Generates .nfo metadata files
/// 6. Removes the "rename" tag when done
pub struct RenameService {
    db: SqlitePool,
    downloader: Arc<DownloaderService>,
    parser: Parser,
    /// Maximum number of concurrent rename tasks
    concurrency: usize,
}

impl RenameService {
    /// Create a new RenameService with default concurrency of 4
    pub fn new(db: SqlitePool, downloader: Arc<DownloaderService>) -> Self {
        Self {
            db,
            downloader,
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

        stream::iter(pending)
            .map(|(task, torrent, bangumi)| async move {
                if let Err(e) = self.process_task(&task, &torrent, &bangumi).await {
                    tracing::error!("Failed to rename task '{}' ({}): {}", task.name, task.id, e);
                }
            })
            .buffer_unordered(self.concurrency)
            .collect::<Vec<_>>()
            .await;

        Ok(())
    }

    /// Get all completed tasks with "rename" tag
    async fn get_pending_tasks(&self) -> Result<Vec<(Task, Torrent, Bangumi)>> {
        // Query downloader for completed/seeding tasks with "rename" tag
        let filter = TaskFilter::new()
            .statuses([TaskStatus::Completed, TaskStatus::Seeding])
            .tag("rename");

        let all_tasks = self.downloader.get_tasks(Some(&filter)).await?;

        let mut result = Vec::new();

        for task in all_tasks {
            // Find matching torrent in database by info_hash
            if let Some(torrent) = TorrentRepository::get_by_info_hash(&self.db, &task.id).await? {
                // Get associated bangumi
                if let Some(bangumi) =
                    BangumiRepository::get_by_id(&self.db, torrent.bangumi_id).await?
                {
                    result.push((task, torrent, bangumi));
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
    async fn process_task(&self, task: &Task, torrent: &Torrent, bangumi: &Bangumi) -> Result<()> {
        tracing::info!(
            "Processing task: {} ({}) for bangumi: {}",
            task.name,
            task.id,
            bangumi.title_chinese
        );

        // Get file list from downloader
        let files = self.downloader.get_task_files(&task.id).await?;

        // Filter to video files only
        let video_files: Vec<_> = files.iter().filter(|f| f.is_video()).collect();

        if video_files.is_empty() {
            tracing::warn!("No video files found in task: {}", task.name);
            // Still remove the tag since there's nothing to rename
            self.finalize_task(&task.id).await?;
            return Ok(());
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
                self.rename_file(task, video_file, bangumi, ep, &files)
                    .await?;
            } else {
                tracing::warn!("Could not determine episode number for: {}", video_file.path);
            }
        }

        // Remove the "rename" tag
        self.finalize_task(&task.id).await?;

        tracing::info!("Successfully renamed task: {}", task.name);
        Ok(())
    }

    /// Rename a single video file and associated subtitles
    async fn rename_file(
        &self,
        task: &Task,
        file: &TaskFile,
        bangumi: &Bangumi,
        episode: i32,
        all_files: &[TaskFile],
    ) -> Result<()> {
        let old_path = &file.path;

        // Get file extension
        let ext = file.extension().unwrap_or("mkv");

        // Generate new filename using pathgen
        let new_filename_base = pathgen::generate_filename(
            &bangumi.title_chinese,
            bangumi.season,
            episode,
            Some(bangumi.platform.as_str()),
        );

        let new_filename = format!("{}.{}", new_filename_base, ext);

        // Preserve directory structure
        let old_path_obj = Path::new(old_path);
        let new_path = if let Some(parent) = old_path_obj.parent() {
            if parent.as_os_str().is_empty() {
                new_filename.clone()
            } else {
                parent.join(&new_filename).to_string_lossy().to_string()
            }
        } else {
            new_filename.clone()
        };

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
        self.write_nfo_file(
            &nfo_dir.to_string_lossy(),
            &new_filename_base,
            bangumi,
            episode,
            old_path,
        )
        .await?;

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
            let new_subtitle_path = if let Some(parent) = file_path.parent() {
                if parent.as_os_str().is_empty() {
                    new_subtitle_name.clone()
                } else {
                    parent
                        .join(&new_subtitle_name)
                        .to_string_lossy()
                        .to_string()
                }
            } else {
                new_subtitle_name.clone()
            };

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
        bangumi: &Bangumi,
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
    fn generate_nfo(bangumi: &Bangumi, episode: i32, original_filename: &str) -> String {
        let mut nfo =
            String::from("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
        nfo.push_str("<episodedetails>\n");

        // Title and show info
        nfo.push_str(&format!("  <title>Episode {}</title>\n", episode));
        nfo.push_str(&format!(
            "  <showtitle>{}</showtitle>\n",
            Self::escape_xml(&bangumi.title_chinese)
        ));

        // Season and episode
        nfo.push_str(&format!("  <season>{}</season>\n", bangumi.season));
        nfo.push_str(&format!("  <episode>{}</episode>\n", episode));

        // Year and air date
        nfo.push_str(&format!("  <year>{}</year>\n", bangumi.year));
        nfo.push_str(&format!("  <aired>{}</aired>\n", bangumi.air_date));

        // IDs
        if let Some(tmdb_id) = bangumi.tmdb_id {
            nfo.push_str(&format!("  <tmdbid>{}</tmdbid>\n", tmdb_id));
            nfo.push_str(&format!(
                "  <uniqueid type=\"tmdb\" default=\"true\">{}</uniqueid>\n",
                tmdb_id
            ));
        }
        if let Some(bgmtv_id) = bangumi.bgmtv_id {
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

    /// Remove the "rename" tag from a task
    async fn finalize_task(&self, task_id: &str) -> Result<()> {
        self.downloader.remove_tags(task_id, &["rename"]).await?;
        Ok(())
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
        use crate::models::{Platform, SourceType};

        let bangumi = Bangumi {
            id: 1,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            title_chinese: "测试动画".to_string(),
            title_japanese: None,
            title_original_chinese: "测试动画".to_string(),
            title_original_japanese: None,
            season: 1,
            year: 2024,
            bgmtv_id: Some(12345),
            tmdb_id: Some(67890),
            poster_url: None,
            air_date: "2024-01-01".to_string(),
            air_week: 1,
            total_episodes: 12,
            episode_offset: 0,
            current_episode: 0,
            auto_complete: true,
            save_path: "/downloads".to_string(),
            source_type: SourceType::WebRip,
            finished: false,
            platform: Platform::Tv,
        };

        let nfo = RenameService::generate_nfo(&bangumi, 5, "original.mkv");

        assert!(nfo.contains("<episode>5</episode>"));
        assert!(nfo.contains("<season>1</season>"));
        assert!(nfo.contains("<tmdbid>67890</tmdbid>"));
        assert!(nfo.contains("<showtitle>测试动画</showtitle>"));
    }
}
