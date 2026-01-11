//! BDRip (Blu-ray Disc Rip) task processing
//!
//! Handles renaming of BDRip releases which have complex directory structures
//! including seasons, specials (SPs), and extras.
//!
//! This processor is shared between tracked and untracked torrent processing.

use parser::{BDRipContentType, BDRipParser};

use crate::models::{Bangumi, SourceType};
use crate::services::{DownloaderHandle, Task, TaskFile};

use super::utils::{rename_bdrip_episode, rename_bdrip_special};
use super::Result;

/// BDRip rename processor
pub(super) struct BDRipProcessor;

impl BDRipProcessor {
    /// Check if a task is a BDRip (three-way detection)
    ///
    /// Detection methods:
    /// 1. User marked source_type as BDRip
    /// 2. Torrent title contains "bdrip" (case insensitive)
    /// 3. Directory structure contains BDRip indicators (SPs, CDs, Scans)
    pub(super) fn is_bdrip(
        task: &Task,
        bangumi: &Bangumi,
        files: &[TaskFile],
    ) -> bool {
        // 1. User marked as BDRip
        if bangumi.source_type == SourceType::BDRip {
            return true;
        }

        // 2. Torrent title contains "bdrip"
        if task.name.to_lowercase().contains("bdrip") {
            return true;
        }

        // 3. Directory structure detection
        Self::detect_structure(files)
    }

    /// Check if file structure indicates BDRip content
    ///
    /// Detects BDRip by looking for characteristic directories:
    /// - SPs/SP/Specials: Special episodes
    /// - CDs/CD: Music/OST
    /// - Scans/Scan: Booklet scans
    fn detect_structure(files: &[TaskFile]) -> bool {
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

    /// Process a BDRip task with complex directory structure
    ///
    /// This method is used by both tracked and untracked torrent processing.
    /// Season priority: file path parsed season > bangumi.season
    pub(super) async fn process(
        downloader: &DownloaderHandle,
        task: &Task,
        video_files: &[&TaskFile],
        bangumi: &Bangumi,
        all_files: &[TaskFile],
    ) -> Result<Vec<i32>> {
        let mut renamed_episodes = Vec::new();

        for video_file in video_files {
            let parse_result = BDRipParser::parse(&video_file.path);

            match parse_result.content_type {
                BDRipContentType::NonVideo => {
                    // Skip non-video content (shouldn't happen as we filtered video files)
                    tracing::debug!("Skipping non-video in BDRip: {}", video_file.path);
                    continue;
                }
                BDRipContentType::Special => {
                    // Process special/SP content
                    if let Some(sp_number) = parse_result.number {
                        if rename_bdrip_special(
                            downloader,
                            task,
                            video_file,
                            bangumi,
                            sp_number,
                            all_files,
                        )
                        .await
                        .is_ok()
                        {
                            tracing::info!(
                                "Renamed special SP{:02} for: {}",
                                sp_number,
                                video_file.path
                            );
                        }
                    } else {
                        tracing::warn!(
                            "Could not determine special number for: {}",
                            video_file.path
                        );
                    }
                }
                BDRipContentType::Episode => {
                    // Process regular episode
                    let season = parse_result.season.unwrap_or(bangumi.season);

                    if let Some(episode) = parse_result.number {
                        if rename_bdrip_episode(
                            downloader,
                            task,
                            video_file,
                            bangumi,
                            season,
                            episode,
                            all_files,
                        )
                        .await
                        .is_ok()
                        {
                            renamed_episodes.push(episode);
                        }
                    } else {
                        tracing::warn!(
                            "Could not determine episode number for BDRip: {}",
                            video_file.path
                        );
                    }
                }
            }
        }

        Ok(renamed_episodes)
    }
}
