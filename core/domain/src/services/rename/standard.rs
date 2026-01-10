//! Standard (WebRip) rename processing.
//!
//! Handles typical single-file or multi-file episode downloads from RSS feeds.

use parser::Parser;
use std::path::Path;
use std::sync::Arc;

use crate::models::{BangumiWithSeries, Torrent};
use crate::services::{DownloaderHandle, Task, TaskFile};

use super::paths::join_with_parent;
use super::Result;

/// Standard rename processor for WebRip content
pub struct StandardProcessor {
    downloader: Arc<DownloaderHandle>,
    parser: Parser,
}

impl StandardProcessor {
    pub fn new(downloader: Arc<DownloaderHandle>) -> Self {
        Self {
            downloader,
            parser: Parser::new(),
        }
    }

    /// Process a standard (non-BDRip) task
    pub async fn process(
        &self,
        task: &Task,
        video_files: &[&TaskFile],
        torrent: &Torrent,
        bangumi: &BangumiWithSeries,
        all_files: &[TaskFile],
    ) -> Result<Vec<i32>> {
        let mut renamed_episodes = Vec::new();

        // Try to parse episode number from torrent_url first (for single-file fallback)
        let torrent_episode = self.parse_episode_number(&torrent.torrent_url);

        for video_file in video_files {
            // If single video file, try torrent_url episode as fallback
            let episode = if video_files.len() == 1 {
                self.parse_episode_number(&video_file.path)
                    .or(torrent_episode)
            } else {
                // Multiple files - always parse from filename
                self.parse_episode_number(&video_file.path)
            };

            if let Some(ep) = episode {
                if self
                    .rename_file(task, video_file, bangumi, ep, all_files)
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

        Ok(renamed_episodes)
    }

    /// Rename a single video file and associated subtitles
    async fn rename_file(
        &self,
        task: &Task,
        file: &TaskFile,
        bangumi: &BangumiWithSeries,
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
            &bangumi.bangumi.title_chinese,
            bangumi.bangumi.season,
            adjusted_episode,
            Some(bangumi.bangumi.platform.as_str()),
        );

        let new_filename = format!("{}.{}", new_filename_base, ext);

        // Preserve directory structure
        let new_path = join_with_parent(old_path, &new_filename);

        // Rename associated subtitle files FIRST (before video rename to avoid path cache issues)
        super::subtitles::rename_subtitles(
            &self.downloader,
            task,
            old_path,
            &new_filename_base,
            all_files,
        )
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
}
