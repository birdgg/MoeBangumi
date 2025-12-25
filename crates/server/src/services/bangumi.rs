use sqlx::SqlitePool;
use std::sync::Arc;
use thiserror::Error;

use crate::models::{
    Bangumi, BangumiWithRss, Clearable, CreateBangumi, CreateRss, RssEntry, UpdateBangumi,
    UpdateBangumiRequest,
};
use crate::repositories::{BangumiRepository, RssRepository};
use crate::services::PosterService;

#[derive(Debug, Error)]
pub enum BangumiError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Bangumi not found")]
    NotFound,
}

/// Service for managing Bangumi entities and their RSS subscriptions
pub struct BangumiService {
    db: SqlitePool,
    poster: Arc<PosterService>,
}

impl BangumiService {
    /// Create a new BangumiService
    pub fn new(db: SqlitePool, poster: Arc<PosterService>) -> Self {
        Self { db, poster }
    }

    /// Create a new bangumi with optional RSS subscriptions
    pub async fn create(&self, mut data: CreateBangumi) -> Result<Bangumi, BangumiError> {
        // Extract RSS entries before creating bangumi
        let rss_entries = std::mem::take(&mut data.rss_entries);

        // Try to download poster if available
        if let Some(ref poster_url) = data.poster_url {
            if let Some(local_path) = self.poster.try_download(poster_url).await {
                data.poster_url = Some(local_path);
            }
        }

        // Create bangumi
        let bangumi = BangumiRepository::create(&self.db, data).await?;

        // Create RSS subscriptions
        for entry in rss_entries {
            let create_rss = CreateRss {
                bangumi_id: bangumi.id,
                url: entry.url,
                enabled: true,
                exclude_filters: entry.filters,
                is_primary: entry.is_primary,
            };

            if let Err(e) = RssRepository::create(&self.db, create_rss).await {
                tracing::error!("Failed to create RSS subscription: {}", e);
            }
        }

        Ok(bangumi)
    }

    /// Get all bangumi
    pub async fn get_all(&self) -> Result<Vec<Bangumi>, BangumiError> {
        Ok(BangumiRepository::get_all(&self.db).await?)
    }

    /// Get a bangumi by ID with its RSS subscriptions
    pub async fn get_with_rss(&self, id: i64) -> Result<BangumiWithRss, BangumiError> {
        let bangumi = BangumiRepository::get_by_id(&self.db, id)
            .await?
            .ok_or(BangumiError::NotFound)?;

        let rss_entries = RssRepository::get_by_bangumi_id(&self.db, id).await?;

        Ok(BangumiWithRss {
            bangumi,
            rss_entries,
        })
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
            auto_download: request.auto_download,
            save_path: match request.save_path {
                Some(Some(path)) => Clearable::Set(path),
                Some(None) => Clearable::Clear,
                None => Clearable::Unchanged,
            },
            ..Default::default()
        };

        // Update bangumi
        BangumiRepository::update(&self.db, id, update_data).await?;

        // Sync RSS entries if provided
        if let Some(rss_entries) = request.rss_entries {
            self.sync_rss_entries(id, rss_entries).await?;
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
    async fn sync_rss_entries(
        &self,
        bangumi_id: i64,
        entries: Vec<RssEntry>,
    ) -> Result<(), BangumiError> {
        // Delete existing RSS entries
        RssRepository::delete_by_bangumi_id(&self.db, bangumi_id).await?;

        // Create new RSS entries
        for entry in entries {
            let create_rss = CreateRss {
                bangumi_id,
                url: entry.url,
                enabled: true,
                exclude_filters: entry.filters,
                is_primary: entry.is_primary,
            };

            if let Err(e) = RssRepository::create(&self.db, create_rss).await {
                tracing::error!("Failed to create RSS subscription: {}", e);
            }
        }

        Ok(())
    }
}
