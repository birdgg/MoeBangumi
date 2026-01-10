use sqlx::SqlitePool;
use std::collections::HashSet;
use std::sync::Arc;
use thiserror::Error;

use crate::models::{
    format_rss_title, BangumiWithRss, BangumiWithSeries, CreateBangumi, CreateRss, CreateSeries,
    RssEntry, UpdateBangumi, UpdateBangumiRequest,
};
use crate::repositories::{BangumiRepository, CreateBangumiData, RssRepository, SeriesRepository};
use crate::services::{RssProcessingService, SettingsService};

#[derive(Debug, Error)]
pub enum BangumiError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Bangumi not found")]
    NotFound,
    #[error("Path generation failed: {0}")]
    PathGeneration(#[from] pathgen::PathGenError),
    #[error("Missing series: either series_id or series must be provided")]
    MissingSeries,
}

/// Service for managing Bangumi entities and their RSS subscriptions
pub struct BangumiService {
    db: SqlitePool,
    rss_processing: Arc<RssProcessingService>,
    settings: Arc<SettingsService>,
}

impl BangumiService {
    /// Create a new BangumiService
    pub fn new(
        db: SqlitePool,
        rss_processing: Arc<RssProcessingService>,
        settings: Arc<SettingsService>,
    ) -> Self {
        Self {
            db,
            rss_processing,
            settings,
        }
    }

    /// Create a new bangumi with optional RSS subscriptions
    pub async fn create(&self, data: CreateBangumi) -> Result<BangumiWithSeries, BangumiError> {
        // Extract RSS entries
        let rss_entries = data.rss_entries.clone();

        // Get or create series
        let series = if let Some(series_id) = data.series_id {
            // Use existing series
            SeriesRepository::get_by_id(&self.db, series_id)
                .await?
                .ok_or(BangumiError::MissingSeries)?
        } else if let Some(create_series) = data.series {
            // Find or create series
            let (series, _created) =
                SeriesRepository::find_or_create(&self.db, create_series).await?;
            series
        } else {
            // Create series from bangumi data
            let create_series = CreateSeries {
                tmdb_id: None,
                title_chinese: data.title_chinese.clone(),
                title_japanese: data.title_japanese.clone(),
                poster_url: data.poster_url.clone(),
            };
            let (series, _created) =
                SeriesRepository::find_or_create(&self.db, create_series).await?;
            series
        };

        // Create bangumi
        let create_data = CreateBangumiData {
            series_id: series.id,
            mikan_id: data.mikan_id,
            bgmtv_id: data.bgmtv_id,
            title_chinese: data.title_chinese,
            title_japanese: data.title_japanese,
            season: data.season,
            year: data.year,
            total_episodes: data.total_episodes,
            poster_url: data.poster_url,
            air_date: data.air_date,
            air_week: data.air_week,
            platform: data.platform.as_str().to_string(),
            current_episode: data.current_episode,
            episode_offset: data.episode_offset,
            auto_complete: data.auto_complete,
            source_type: data.source_type.as_str().to_string(),
        };

        let bangumi = BangumiRepository::create(&self.db, create_data).await?;

        // Collect newly created RSS IDs for background processing
        let mut new_rss_ids = Vec::new();

        // Create RSS subscriptions
        for entry in rss_entries {
            // Generate title: [subtitle_group] {bangumi} S{season} or {bangumi} S{season}
            let title = format_rss_title(
                &bangumi.title_chinese,
                bangumi.season,
                entry.subtitle_group.as_deref(),
            );

            let create_rss = CreateRss {
                bangumi_id: bangumi.id,
                title,
                url: entry.url,
                enabled: true,
                exclude_filters: entry.filters,
                include_filters: entry.include_filters,
                subtitle_group: entry.subtitle_group,
            };

            match RssRepository::create(&self.db, create_rss).await {
                Ok(rss) => {
                    new_rss_ids.push(rss.id);
                }
                Err(e) => {
                    tracing::error!("Failed to create RSS subscription: {}", e);
                }
            }
        }

        // Trigger background RSS fetch for newly created subscriptions
        if !new_rss_ids.is_empty() {
            self.rss_processing.spawn_background(new_rss_ids);
        }

        Ok(BangumiWithSeries { bangumi, series })
    }

    /// Get all bangumi with series
    pub async fn get_all(&self) -> Result<Vec<BangumiWithSeries>, BangumiError> {
        Ok(BangumiRepository::get_all_with_series(&self.db).await?)
    }

    /// Get a bangumi by ID with series and RSS subscriptions
    pub async fn get_with_rss(&self, id: i64) -> Result<BangumiWithRss, BangumiError> {
        let bangumi_with_series = BangumiRepository::get_with_series_by_id(&self.db, id)
            .await?
            .ok_or(BangumiError::NotFound)?;

        let rss_entries = RssRepository::get_by_bangumi_id(&self.db, id).await?;

        Ok(BangumiWithRss {
            bangumi: bangumi_with_series.bangumi,
            series: bangumi_with_series.series,
            rss_entries,
        })
    }

    /// Get a bangumi by ID with series
    pub async fn get_by_id(&self, id: i64) -> Result<BangumiWithSeries, BangumiError> {
        BangumiRepository::get_with_series_by_id(&self.db, id)
            .await?
            .ok_or(BangumiError::NotFound)
    }

    /// Update a bangumi with optional RSS synchronization
    pub async fn update(
        &self,
        id: i64,
        request: UpdateBangumiRequest,
    ) -> Result<BangumiWithRss, BangumiError> {
        // Check if bangumi exists
        let _bangumi = BangumiRepository::get_by_id(&self.db, id)
            .await?
            .ok_or(BangumiError::NotFound)?;

        // Build update data
        let update_data = UpdateBangumi {
            auto_complete: request.auto_complete,
            episode_offset: request.episode_offset,
            ..Default::default()
        };

        // Update bangumi
        BangumiRepository::update(&self.db, id, update_data).await?;

        // Sync RSS entries if provided
        if let Some(rss_entries) = request.rss_entries {
            let new_rss_ids = self.sync_rss_entries(id, rss_entries).await?;

            // Trigger background RSS fetch for newly added subscriptions
            if !new_rss_ids.is_empty() {
                self.rss_processing.spawn_background(new_rss_ids);
            }
        }

        // Return updated bangumi with RSS
        self.get_with_rss(id).await
    }

    /// Delete a bangumi and its RSS subscriptions
    pub async fn delete(&self, id: i64) -> Result<bool, BangumiError> {
        // Delete RSS entries first (foreign key constraint)
        RssRepository::delete_by_bangumi_id(&self.db, id).await?;

        // Delete bangumi
        Ok(BangumiRepository::delete(&self.db, id).await?)
    }

    /// Generate save path for a bangumi
    pub fn generate_save_path(&self, bangumi: &BangumiWithSeries) -> Result<String, BangumiError> {
        let settings = self.settings.get();
        let base_path = &settings.downloader.save_path;

        let save_path = pathgen::generate_directory(
            base_path,
            &bangumi.series.title_chinese,
            bangumi.bangumi.year,
            bangumi.bangumi.season,
            bangumi.series.tmdb_id,
            Some(bangumi.bangumi.platform.as_str()),
        )?;

        Ok(save_path)
    }

    /// Synchronize RSS entries for a bangumi (delete all and recreate)
    /// Returns the IDs of newly added RSS subscriptions
    async fn sync_rss_entries(
        &self,
        bangumi_id: i64,
        entries: Vec<RssEntry>,
    ) -> Result<Vec<i64>, BangumiError> {
        // Get bangumi with series info to generate RSS titles
        let bangumi_with_series = BangumiRepository::get_with_series_by_id(&self.db, bangumi_id)
            .await?
            .ok_or(BangumiError::NotFound)?;

        let bangumi = &bangumi_with_series.bangumi;

        // Get existing RSS URLs to identify new ones
        let existing_rss = RssRepository::get_by_bangumi_id(&self.db, bangumi_id).await?;
        let existing_urls: HashSet<_> = existing_rss.iter().map(|r| r.url.as_str()).collect();

        // Delete existing RSS entries
        RssRepository::delete_by_bangumi_id(&self.db, bangumi_id).await?;

        // Create new RSS entries and track newly added ones
        let mut new_rss_ids = Vec::new();

        for entry in entries {
            let is_new = !existing_urls.contains(entry.url.as_str());

            // Generate title: [subtitle_group] {bangumi} S{season} or {bangumi} S{season}
            let title = format_rss_title(
                &bangumi.title_chinese,
                bangumi.season,
                entry.subtitle_group.as_deref(),
            );

            let create_rss = CreateRss {
                bangumi_id,
                title,
                url: entry.url,
                enabled: true,
                exclude_filters: entry.filters,
                include_filters: entry.include_filters,
                subtitle_group: entry.subtitle_group,
            };

            match RssRepository::create(&self.db, create_rss).await {
                Ok(rss) => {
                    if is_new {
                        new_rss_ids.push(rss.id);
                    }
                }
                Err(e) => {
                    tracing::error!("Failed to create RSS subscription: {}", e);
                }
            }
        }

        Ok(new_rss_ids)
    }
}
