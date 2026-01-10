//! BDRip-specific rename processing.
//!
//! Handles complex directory structures typically found in BDRip releases:
//! - Multiple seasons in a single torrent
//! - Special episodes (SPs, OVAs)
//! - CDs, Scans, and other non-video content

use parser::{BDRipContentType, BDRipParser};
use std::sync::Arc;

use crate::models::BangumiWithSeries;
use crate::services::{DownloaderHandle, Task, TaskFile};

use super::paths::{build_bdrip_path, build_special_path};
use super::Result;

/// BDRip rename processor
pub struct BDRipProcessor {
    downloader: Arc<DownloaderHandle>,
}

impl BDRipProcessor {
    pub fn new(downloader: Arc<DownloaderHandle>) -> Self {
        Self { downloader }
    }

    /// Process a BDRip task with complex directory structure
    /// Supports multi-season BDRip by matching files to correct bangumi by season
    pub async fn process(
        &self,
        task: &Task,
        video_files: &[&TaskFile],
        all_bangumi: &[BangumiWithSeries],
        all_files: &[TaskFile],
    ) -> Result<Vec<i32>> {
        let mut renamed_episodes = Vec::new();

        // Get primary bangumi for fallback (first season)
        let primary_bangumi = all_bangumi
            .first()
            .ok_or_else(|| super::RenameError::BangumiNotFound(0))?;

        for video_file in video_files {
            let parse_result = BDRipParser::parse(&video_file.path);

            match parse_result.content_type {
                BDRipContentType::NonVideo => {
                    // Skip non-video content (shouldn't happen as we filtered video files)
                    tracing::debug!("Skipping non-video in BDRip: {}", video_file.path);
                    continue;
                }
                BDRipContentType::Special => {
                    // Process special/SP content (use primary bangumi for specials)
                    if let Some(sp_number) = parse_result.number {
                        if self
                            .rename_special(task, video_file, primary_bangumi, sp_number, all_files)
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
                    let season = parse_result.season.unwrap_or(primary_bangumi.bangumi.season);

                    // Match bangumi by season for multi-season BDRip support
                    let bangumi =
                        match_bangumi_by_season(season, all_bangumi).unwrap_or(primary_bangumi);

                    if let Some(episode) = parse_result.number {
                        if self
                            .rename_episode(task, video_file, bangumi, season, episode, all_files)
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

    /// Rename a BDRip episode file
    ///
    /// Note: Unlike standard WebRip processing, BDRip episode numbers are NOT
    /// adjusted with episode_offset. This is because:
    /// 1. BDRip releases typically use season-relative numbering (e.g., S2E01, not S2E13)
    /// 2. The episode number is parsed from directory structure, not RSS metadata
    /// 3. episode_offset is designed for RSS feeds that use absolute episode numbers
    async fn rename_episode(
        &self,
        task: &Task,
        file: &TaskFile,
        bangumi: &BangumiWithSeries,
        season: i32,
        episode: i32,
        all_files: &[TaskFile],
    ) -> Result<()> {
        let ext = file.extension().unwrap_or("mkv");

        // BDRip episode numbers are already season-relative, no offset needed
        let new_filename_base = pathgen::generate_filename(
            &bangumi.bangumi.title_chinese,
            season,
            episode,
            Some(bangumi.bangumi.platform.as_str()),
        );

        // Build the full destination path
        let new_path = build_bdrip_path(
            &bangumi.series.title_chinese,
            bangumi.bangumi.year,
            bangumi.series.tmdb_id,
            season,
            &new_filename_base,
            ext,
        );

        // Rename subtitles first
        super::subtitles::rename_subtitles(
            &self.downloader,
            task,
            &file.path,
            &new_filename_base,
            all_files,
        )
        .await?;

        // Rename video file
        if file.path != new_path {
            tracing::info!("BDRip rename: {} -> {}", file.path, new_path);
            self.downloader
                .rename_file(&task.id, &file.path, &new_path)
                .await?;
        }

        Ok(())
    }

    /// Rename a special/SP file
    async fn rename_special(
        &self,
        task: &Task,
        file: &TaskFile,
        bangumi: &BangumiWithSeries,
        sp_number: i32,
        all_files: &[TaskFile],
    ) -> Result<()> {
        let ext = file.extension().unwrap_or("mkv");

        // Generate special filename: Title - s00eXX
        let title = pathgen::sanitize(&bangumi.bangumi.title_chinese);
        let new_filename_base = format!("{} - s00e{:02}", title, sp_number);

        // Build the full destination path with Specials directory
        let new_path = build_special_path(
            &bangumi.series.title_chinese,
            bangumi.bangumi.year,
            bangumi.series.tmdb_id,
            &new_filename_base,
            ext,
        );

        // Rename subtitles first
        super::subtitles::rename_subtitles(
            &self.downloader,
            task,
            &file.path,
            &new_filename_base,
            all_files,
        )
        .await?;

        // Rename video file
        if file.path != new_path {
            tracing::info!("Special rename: {} -> {}", file.path, new_path);
            self.downloader
                .rename_file(&task.id, &file.path, &new_path)
                .await?;
        }

        Ok(())
    }
}

/// Match bangumi by season number for multi-season BDRip support
pub fn match_bangumi_by_season<'a>(
    season: i32,
    all_bangumi: &'a [BangumiWithSeries],
) -> Option<&'a BangumiWithSeries> {
    all_bangumi.iter().find(|b| b.bangumi.season == season)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{Bangumi, Platform, Series, SourceType};
    use chrono::Utc;

    /// Helper to create a test BangumiWithSeries
    fn create_test_bangumi(
        id: i64,
        season: i32,
        year: i32,
        title: &str,
        tmdb_id: Option<i64>,
    ) -> BangumiWithSeries {
        BangumiWithSeries {
            bangumi: Bangumi {
                id,
                created_at: Utc::now(),
                updated_at: Utc::now(),
                series_id: 1,
                mikan_id: None,
                bgmtv_id: None,
                title_chinese: title.to_string(),
                title_japanese: None,
                season,
                year,
                total_episodes: 12,
                poster_url: None,
                air_date: None,
                air_week: 0,
                platform: Platform::Tv,
                current_episode: 0,
                episode_offset: 0,
                auto_complete: true,
                source_type: SourceType::BDRip,
            },
            series: Series {
                id: 1,
                created_at: Utc::now(),
                updated_at: Utc::now(),
                tmdb_id,
                title_chinese: title.to_string(),
                title_japanese: None,
                poster_url: None,
            },
        }
    }

    #[test]
    fn test_match_bangumi_by_season_single() {
        let bangumi_list = vec![create_test_bangumi(1, 1, 2016, "Re:Zero", Some(63926))];

        // Match existing season
        let result = match_bangumi_by_season(1, &bangumi_list);
        assert!(result.is_some());
        assert_eq!(result.unwrap().bangumi.season, 1);

        // Non-existing season returns None
        let result = match_bangumi_by_season(2, &bangumi_list);
        assert!(result.is_none());
    }

    #[test]
    fn test_match_bangumi_by_season_multi() {
        let bangumi_list = vec![
            create_test_bangumi(1, 1, 2016, "Re:Zero S1", Some(63926)),
            create_test_bangumi(2, 2, 2020, "Re:Zero S2", Some(63926)),
            create_test_bangumi(3, 3, 2024, "Re:Zero S3", Some(63926)),
        ];

        // Match season 1
        let result = match_bangumi_by_season(1, &bangumi_list);
        assert!(result.is_some());
        assert_eq!(result.unwrap().bangumi.id, 1);
        assert_eq!(result.unwrap().bangumi.year, 2016);

        // Match season 2
        let result = match_bangumi_by_season(2, &bangumi_list);
        assert!(result.is_some());
        assert_eq!(result.unwrap().bangumi.id, 2);
        assert_eq!(result.unwrap().bangumi.year, 2020);

        // Match season 3
        let result = match_bangumi_by_season(3, &bangumi_list);
        assert!(result.is_some());
        assert_eq!(result.unwrap().bangumi.id, 3);
        assert_eq!(result.unwrap().bangumi.year, 2024);

        // Non-existing season
        let result = match_bangumi_by_season(4, &bangumi_list);
        assert!(result.is_none());
    }

    #[test]
    fn test_match_bangumi_by_season_empty() {
        let bangumi_list: Vec<BangumiWithSeries> = vec![];
        let result = match_bangumi_by_season(1, &bangumi_list);
        assert!(result.is_none());
    }
}
