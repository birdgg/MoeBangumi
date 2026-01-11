//! Standard (WebRip) task processing
//!
//! Handles renaming of standard torrent downloads where files follow
//! typical anime naming conventions.

use parser::Parser;

use crate::models::{BangumiWithMetadata, Torrent};
use crate::services::{DownloaderHandle, Task, TaskFile};

use super::utils::{generate_filename_base, generate_standard_path, parse_episode_number, rename_subtitles};
use super::Result;

/// Standard WebRip rename processor
pub(super) struct StandardProcessor;

impl StandardProcessor {
    /// Process a standard (non-BDRip) task
    ///
    /// Iterates through video files, parses episode numbers,
    /// and renames them to Plex/Jellyfin compatible format.
    pub(super) async fn process(
        downloader: &DownloaderHandle,
        parser: &Parser,
        task: &Task,
        video_files: &[&TaskFile],
        torrent: &Torrent,
        bangumi: &BangumiWithMetadata,
        all_files: &[TaskFile],
    ) -> Result<Vec<i32>> {
        let mut renamed_episodes = Vec::new();

        for video_file in video_files {
            // If single video file and torrent has episode_number, use it as fallback
            let episode = if video_files.len() == 1 {
                torrent
                    .episode_number
                    .or_else(|| parse_episode_number(parser, &video_file.path))
            } else {
                // Multiple files - always parse from filename
                parse_episode_number(parser, &video_file.path)
            };

            if let Some(ep) = episode {
                if Self::rename_file(downloader, task, video_file, bangumi, ep, all_files)
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
        downloader: &DownloaderHandle,
        task: &Task,
        file: &TaskFile,
        bangumi: &BangumiWithMetadata,
        episode: i32,
        all_files: &[TaskFile],
    ) -> Result<()> {
        let old_path = &file.path;

        // Get file extension
        let ext = file.extension().unwrap_or("mkv");

        // Generate new filename using pathgen with episode offset applied
        let new_filename_base = generate_filename_base(bangumi, episode);
        let new_path = generate_standard_path(bangumi, old_path, episode, ext);

        // Rename associated subtitle files FIRST (before video rename to avoid path cache issues)
        rename_subtitles(downloader, task, old_path, &new_filename_base, all_files).await?;

        // Now rename the video file
        if old_path == &new_path {
            tracing::debug!("File already has correct name: {}", old_path);
        } else {
            tracing::info!("Renaming: {} -> {}", old_path, new_path);
            downloader
                .rename_file(&task.id, old_path, &new_path)
                .await?;
        }

        Ok(())
    }
}
