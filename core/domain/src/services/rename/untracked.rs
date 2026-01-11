//! Untracked task processing
//!
//! Handles renaming of torrents that are not tracked in the database,
//! using metadata resolved from BGM.tv/TMDB or fallback to parsed titles.

use parser::Parser;

use crate::models::BangumiWithMetadata;
use crate::services::{DownloaderHandle, Task, TaskFile};
use crate::utils::is_temp_download_path;

use super::bdrip::BDRipProcessor;
use super::utils::{build_bdrip_path, join_with_parent, parse_episode_number, rename_subtitles};
use super::{RenameTaskResult, Result};

/// Untracked torrent rename processor
pub(super) struct UntrackedProcessor;

impl UntrackedProcessor {
    /// Process an untracked task (torrent not in database but metadata resolved)
    ///
    /// Similar to tracked task processing but without torrent context.
    /// Uses parsed episode numbers from filenames.
    pub(super) async fn process(
        downloader: &DownloaderHandle,
        parser: &Parser,
        task: &Task,
        bangumi: &BangumiWithMetadata,
    ) -> Result<RenameTaskResult> {
        tracing::info!(
            "Processing untracked task: {} ({}) for bangumi: {}",
            task.name,
            task.id,
            bangumi.metadata.title_chinese
        );

        // Get file list from downloader
        let files = downloader.get_task_files(&task.id).await?;

        // Filter to video files only
        let video_files: Vec<_> = files.iter().filter(|f| f.is_video()).collect();

        if video_files.is_empty() {
            tracing::warn!("No video files found in untracked task: {}", task.name);
            return Ok(RenameTaskResult {
                bangumi_id: bangumi.bangumi.id,
                bangumi_title: bangumi.metadata.title_chinese.clone(),
                poster_url: bangumi.metadata.poster_url.clone(),
                total_episodes: bangumi.metadata.total_episodes,
                renamed_episodes: Vec::new(),
            });
        }

        // Check if this is a BDRip task
        let is_bdrip = BDRipProcessor::is_bdrip(task, bangumi, &files);

        let renamed_episodes = if is_bdrip {
            tracing::info!(
                "Detected BDRip structure for untracked task: {}",
                task.name
            );
            // Delegate to BDRipProcessor - same logic as tracked BDRip tasks
            BDRipProcessor::process(downloader, task, &video_files, bangumi, &files).await?
        } else {
            Self::process_standard(downloader, parser, task, &video_files, bangumi, &files).await?
        };

        // Move task from temp directory if needed
        if is_temp_download_path(&task.save_path) {
            let final_path = &bangumi.bangumi.save_path;
            tracing::info!(
                "Moving untracked task from {} to {}",
                task.save_path,
                final_path
            );
            match downloader.set_location(&task.id, final_path).await {
                Ok(()) => tracing::info!("Successfully moved task {} to {}", task.id, final_path),
                Err(e) => tracing::error!("Failed to move task {}: {}", task.id, e),
            }
        }

        tracing::info!("Successfully renamed untracked task: {}", task.name);
        Ok(RenameTaskResult {
            bangumi_id: bangumi.bangumi.id,
            bangumi_title: bangumi.metadata.title_chinese.clone(),
            poster_url: bangumi.metadata.poster_url.clone(),
            total_episodes: bangumi.metadata.total_episodes,
            renamed_episodes,
        })
    }

    /// Process untracked task with standard (non-BDRip) structure
    async fn process_standard(
        downloader: &DownloaderHandle,
        parser: &Parser,
        task: &Task,
        video_files: &[&TaskFile],
        bangumi: &BangumiWithMetadata,
        all_files: &[TaskFile],
    ) -> Result<Vec<i32>> {
        let mut renamed_episodes = Vec::new();

        for video_file in video_files {
            // Parse episode number from filename
            let episode = parse_episode_number(parser, &video_file.path);

            if let Some(ep) = episode {
                // Parse season from filename, fallback to metadata season
                let season = parser
                    .parse(&video_file.path)
                    .ok()
                    .and_then(|r| r.season)
                    .unwrap_or(bangumi.metadata.season);

                let ext = video_file.extension().unwrap_or("mkv");

                // Generate new filename
                let new_filename_base = pathgen::generate_filename(
                    &bangumi.metadata.title_chinese,
                    season,
                    ep,
                    Some(bangumi.metadata.platform.as_str()),
                );

                // Build destination path with season folder
                let new_path = build_bdrip_path(
                    &bangumi.metadata.title_chinese,
                    bangumi.metadata.year,
                    bangumi.metadata.tmdb_id,
                    season,
                    &new_filename_base,
                    ext,
                );

                // Rename subtitles first
                rename_subtitles(downloader, task, &video_file.path, &new_filename_base, all_files)
                    .await?;

                // Rename video file
                if video_file.path != new_path {
                    tracing::info!("Untracked rename: {} -> {}", video_file.path, new_path);
                    downloader
                        .rename_file(&task.id, &video_file.path, &new_path)
                        .await?;
                    renamed_episodes.push(ep);
                }
            } else {
                tracing::warn!(
                    "Could not determine episode number for untracked file: {}",
                    video_file.path
                );
            }
        }

        Ok(renamed_episodes)
    }

    /// Process a fallback task (no metadata found, use parsed title)
    pub(super) async fn process_fallback(
        downloader: &DownloaderHandle,
        parser: &Parser,
        task: &Task,
        title: &str,
        parsed_season: Option<i32>,
    ) -> Result<RenameTaskResult> {
        tracing::info!(
            "Processing fallback task: {} with title '{}'",
            task.name,
            title
        );

        let files = downloader.get_task_files(&task.id).await?;
        let video_files: Vec<_> = files.iter().filter(|f| f.is_video()).collect();

        if video_files.is_empty() {
            return Ok(RenameTaskResult {
                bangumi_id: 0,
                bangumi_title: title.to_string(),
                poster_url: None,
                total_episodes: 0,
                renamed_episodes: Vec::new(),
            });
        }

        let season = parsed_season.unwrap_or(1);
        let sanitized_title = pathgen::sanitize(title);
        let mut renamed_episodes = Vec::new();

        for video_file in &video_files {
            let parse_result = match parser.parse(&video_file.path) {
                Ok(r) => r,
                Err(_) => continue,
            };

            if let Some(episode) = parse_result.episode {
                let ext = video_file.extension().unwrap_or("mkv");
                let new_filename = format!(
                    "{} - s{:02}e{:02}.{}",
                    sanitized_title, season, episode, ext
                );
                let new_path = join_with_parent(&video_file.path, &new_filename);

                if video_file.path != new_path {
                    tracing::info!("Fallback rename: {} -> {}", video_file.path, new_path);
                    if downloader
                        .rename_file(&task.id, &video_file.path, &new_path)
                        .await
                        .is_ok()
                    {
                        renamed_episodes.push(episode);
                    }
                }
            }
        }

        // Return result with dummy bangumi_id (0 indicates no bangumi created)
        Ok(RenameTaskResult {
            bangumi_id: 0,
            bangumi_title: sanitized_title,
            poster_url: None,
            total_episodes: 0,
            renamed_episodes,
        })
    }
}
