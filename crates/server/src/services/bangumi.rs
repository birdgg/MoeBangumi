use sqlx::SqlitePool;
use std::collections::HashSet;
use std::sync::Arc;
use thiserror::Error;

use crate::models::{
    format_rss_title, BangumiWithMetadata, BangumiWithRss, CreateBangumi, CreateRss, RssEntry,
    UpdateBangumi, UpdateBangumiRequest,
};
use crate::repositories::{BangumiRepository, CreateBangumiData, RssRepository};
use crate::repositories::MetadataRepository;
use crate::services::{MetadataError, MetadataService, PosterService, RssProcessingService, SettingsService};

#[derive(Debug, Error)]
pub enum BangumiError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Bangumi not found")]
    NotFound,
    #[error("Metadata error: {0}")]
    Metadata(#[from] MetadataError),
    #[error("Path generation failed: {0}")]
    PathGeneration(#[from] pathgen::PathGenError),
    #[error("Missing metadata: either metadata_id or metadata must be provided")]
    MissingMetadata,
}

/// Service for managing Bangumi entities and their RSS subscriptions
pub struct BangumiService {
    db: SqlitePool,
    metadata: Arc<MetadataService>,
    poster: Arc<PosterService>,
    rss_processing: Arc<RssProcessingService>,
    settings: Arc<SettingsService>,
}

impl BangumiService {
    /// Create a new BangumiService
    pub fn new(
        db: SqlitePool,
        metadata: Arc<MetadataService>,
        poster: Arc<PosterService>,
        rss_processing: Arc<RssProcessingService>,
        settings: Arc<SettingsService>,
    ) -> Self {
        Self {
            db,
            metadata,
            poster,
            rss_processing,
            settings,
        }
    }

    /// Create a new bangumi with optional RSS subscriptions
    pub async fn create(&self, data: CreateBangumi) -> Result<BangumiWithMetadata, BangumiError> {
        // Extract RSS entries
        let rss_entries = data.rss_entries.clone();

        // Get or create/update metadata
        let mut metadata = if let Some(metadata_id) = data.metadata_id {
            // Use existing metadata
            self.metadata.get_by_id(metadata_id).await?
        } else if let Some(create_metadata) = data.metadata {
            // Create new metadata or update existing by external ID
            self.metadata.find_or_update(create_metadata).await?
        } else {
            return Err(BangumiError::MissingMetadata);
        };

        // Check finished status if metadata has bgmtv_id and is not finished yet
        if let Some(bgmtv_id) = metadata.bgmtv_id {
            if !metadata.finished {
                match self.metadata.check_finished_status(bgmtv_id).await {
                    Ok(Some(true)) => {
                        // Mark as finished in database and refresh metadata to ensure consistency
                        if MetadataRepository::mark_as_finished(&self.db, metadata.id).await.is_ok() {
                            tracing::info!(
                                "Marked metadata {} (bgmtv_id={}, title='{}') as finished during bangumi creation",
                                metadata.id,
                                bgmtv_id,
                                metadata.title_chinese
                            );
                            // Refresh metadata from database to ensure consistency
                            if let Ok(updated) = self.metadata.get_by_id(metadata.id).await {
                                metadata = updated;
                            }
                        }
                    }
                    Ok(Some(false)) => {
                        tracing::debug!(
                            "Metadata {} (bgmtv_id={}, title='{}') is still airing",
                            metadata.id,
                            bgmtv_id,
                            metadata.title_chinese
                        );
                    }
                    Ok(None) => {
                        tracing::debug!(
                            "Cannot determine finish status for metadata {} (bgmtv_id={}, title='{}')",
                            metadata.id,
                            bgmtv_id,
                            metadata.title_chinese
                        );
                    }
                    Err(e) => {
                        // Log error but don't fail the creation
                        tracing::warn!(
                            "Failed to check finish status for metadata {} (bgmtv_id={}): {}",
                            metadata.id,
                            bgmtv_id,
                            e
                        );
                    }
                }
            }
        }

        // Generate save_path using pathgen with metadata
        let settings = self.settings.get();
        let base_path = &settings.downloader.save_path;
        let save_path = pathgen::generate_directory(
            base_path,
            &metadata.title_chinese,
            metadata.year,
            metadata.season,
            metadata.tmdb_id,
            Some(metadata.platform.as_str()),
        )?;

        // Create bangumi
        let create_data = CreateBangumiData {
            metadata_id: metadata.id,
            episode_offset: data.episode_offset,
            auto_complete: data.auto_complete,
            save_path,
            source_type: data.source_type.as_str().to_string(),
        };

        let bangumi = BangumiRepository::create(&self.db, create_data).await?;

        // Collect newly created RSS IDs for background processing
        let mut new_rss_ids = Vec::new();

        // Create RSS subscriptions
        for entry in rss_entries {
            // Generate title: [subtitle_group] {bangumi} S{season} or {bangumi} S{season}
            let title = format_rss_title(
                &metadata.title_chinese,
                metadata.season,
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

        // Trigger background poster download (non-blocking)
        if let Some(ref poster_url) = metadata.poster_url {
            self.poster
                .spawn_download_and_update(metadata.id, poster_url.clone(), self.db.clone());
        }

        Ok(BangumiWithMetadata { bangumi, metadata })
    }

    /// Get all bangumi with metadata
    pub async fn get_all(&self) -> Result<Vec<BangumiWithMetadata>, BangumiError> {
        Ok(BangumiRepository::get_all_with_metadata(&self.db).await?)
    }

    /// Get a bangumi by ID with metadata and RSS subscriptions
    pub async fn get_with_rss(&self, id: i64) -> Result<BangumiWithRss, BangumiError> {
        let bangumi_with_metadata = BangumiRepository::get_with_metadata_by_id(&self.db, id)
            .await?
            .ok_or(BangumiError::NotFound)?;

        let rss_entries = RssRepository::get_by_bangumi_id(&self.db, id).await?;

        Ok(BangumiWithRss {
            bangumi: bangumi_with_metadata.bangumi,
            metadata: bangumi_with_metadata.metadata,
            rss_entries,
        })
    }

    /// Get a bangumi by ID with metadata
    pub async fn get_by_id(&self, id: i64) -> Result<BangumiWithMetadata, BangumiError> {
        BangumiRepository::get_with_metadata_by_id(&self.db, id)
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
        BangumiRepository::get_by_id(&self.db, id)
            .await?
            .ok_or(BangumiError::NotFound)?;

        // Build update data
        let update_data = UpdateBangumi {
            episode_offset: request.episode_offset,
            auto_complete: request.auto_complete,
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

    /// Synchronize RSS entries for a bangumi (delete all and recreate)
    /// Returns the IDs of newly added RSS subscriptions
    async fn sync_rss_entries(
        &self,
        bangumi_id: i64,
        entries: Vec<RssEntry>,
    ) -> Result<Vec<i64>, BangumiError> {
        // Get bangumi with metadata info to generate RSS titles
        let bangumi_with_metadata = BangumiRepository::get_with_metadata_by_id(&self.db, bangumi_id)
            .await?
            .ok_or(BangumiError::NotFound)?;

        let metadata = &bangumi_with_metadata.metadata;

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
                &metadata.title_chinese,
                metadata.season,
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
