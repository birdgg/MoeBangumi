use chrono::{Datelike, Utc};
use mikan::{MikanClient, Season};
use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;
use thiserror::Error;
use tokio::time::sleep;

use crate::repositories::MetadataRepository;

#[derive(Debug, Error)]
pub enum MikanMappingError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Mikan API error: {0}")]
    Mikan(#[from] mikan::MikanError),
}

pub type Result<T> = std::result::Result<T, MikanMappingError>;

/// Service for managing Mikan-BGM.tv ID mappings
/// Stores mappings in the metadata table
pub struct MikanMappingService {
    db: SqlitePool,
    mikan: Arc<MikanClient>,
}

impl MikanMappingService {
    pub fn new(db: SqlitePool, mikan: Arc<MikanClient>) -> Self {
        Self { db, mikan }
    }

    /// Sync the current season's Mikan-BGM.tv mappings
    /// Returns the number of new mappings added
    pub async fn sync_current_season(&self) -> Result<usize> {
        let now = Utc::now();
        let year = now.year();
        let season = Season::current();

        self.sync_season(year, season).await
    }

    /// Sync mappings for a specific season
    pub async fn sync_season(&self, year: i32, season: Season) -> Result<usize> {
        tracing::info!(
            "Starting Mikan mapping sync for {} {}",
            year,
            season.to_chinese()
        );

        // 1. Get the seasonal bangumi list
        let bangumi_list = self.mikan.get_seasonal_bangumi_list(year, season).await?;
        tracing::info!(
            "Found {} bangumi in {} {}",
            bangumi_list.len(),
            year,
            season.to_chinese()
        );

        let mut mappings = Vec::new();
        let mut skipped = 0;
        let mut no_bgmtv = 0;
        let mut errors = 0;

        // 2. Process each bangumi
        for (i, bangumi) in bangumi_list.iter().enumerate() {
            // Check if already exists (avoid redundant requests)
            if MetadataRepository::mikan_id_exists(&self.db, &bangumi.mikan_id).await? {
                skipped += 1;
                tracing::debug!(
                    "Skipping existing mapping for mikan_id={}",
                    bangumi.mikan_id
                );
                continue;
            }

            // Add delay between requests to avoid rate limiting
            if i > 0 {
                sleep(Duration::from_millis(500)).await;
            }

            // Fetch BGM.tv ID from detail page
            match self.mikan.get_bangumi_bgmtv_id(&bangumi.mikan_id).await {
                Ok(Some(bgmtv_id)) => {
                    tracing::debug!(
                        "Found mapping: {} (mikan_id={}) -> bgmtv_id={}",
                        bangumi.name,
                        bangumi.mikan_id,
                        bgmtv_id
                    );
                    // (mikan_id, bgmtv_id, title_chinese, air_week)
                    // SeasonalBangumi doesn't have air_weekday, use 0 as default
                    mappings.push((
                        bangumi.mikan_id.clone(),
                        bgmtv_id,
                        bangumi.name.clone(),
                        0, // air_week unknown from seasonal list
                    ));
                }
                Ok(None) => {
                    no_bgmtv += 1;
                    tracing::debug!(
                        "No BGM.tv link found for: {} (mikan_id={})",
                        bangumi.name,
                        bangumi.mikan_id
                    );
                }
                Err(e) => {
                    errors += 1;
                    tracing::warn!(
                        "Failed to get BGM.tv ID for {} (mikan_id={}): {}",
                        bangumi.name,
                        bangumi.mikan_id,
                        e
                    );
                }
            }
        }

        // 3. Batch insert new mappings into metadata table
        let inserted = MetadataRepository::upsert_batch_mikan(&self.db, &mappings).await?;

        tracing::info!(
            "Mikan mapping sync completed: {} inserted, {} skipped, {} no_bgmtv, {} errors",
            inserted,
            skipped,
            no_bgmtv,
            errors
        );

        Ok(inserted)
    }

    /// Get Mikan ID by BGM.tv ID
    pub async fn get_mikan_id_by_bgmtv(&self, bgmtv_id: i64) -> Result<Option<String>> {
        let metadata = MetadataRepository::get_by_bgmtv_id(&self.db, bgmtv_id).await?;
        Ok(metadata.and_then(|m| m.mikan_id))
    }

    /// Get Mikan ID by TMDB ID
    pub async fn get_mikan_id_by_tmdb(&self, tmdb_id: i64) -> Result<Option<String>> {
        let metadata = MetadataRepository::get_by_tmdb_id(&self.db, tmdb_id).await?;
        Ok(metadata.and_then(|m| m.mikan_id))
    }

    /// Get mapping statistics (count of metadata with mikan_id)
    pub async fn get_stats(&self) -> Result<i64> {
        Ok(MetadataRepository::count(&self.db).await?)
    }
}
