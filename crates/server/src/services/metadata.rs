use sqlx::SqlitePool;
use thiserror::Error;

use crate::models::{CreateMetadata, Metadata, UpdateMetadata};
use crate::repositories::MetadataRepository;

#[derive(Debug, Error)]
pub enum MetadataError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Metadata not found")]
    NotFound,
}

/// Service for managing Metadata entities
pub struct MetadataService {
    db: SqlitePool,
}

impl MetadataService {
    /// Create a new MetadataService
    pub fn new(db: SqlitePool) -> Self {
        Self { db }
    }

    /// Create new metadata
    pub async fn create(&self, data: CreateMetadata) -> Result<Metadata, MetadataError> {
        Ok(MetadataRepository::create(&self.db, data).await?)
    }

    /// Get metadata by ID
    pub async fn get_by_id(&self, id: i64) -> Result<Metadata, MetadataError> {
        MetadataRepository::get_by_id(&self.db, id)
            .await?
            .ok_or(MetadataError::NotFound)
    }

    /// Get metadata by external ID (priority: mikan_id > bgmtv_id > tmdb_id)
    pub async fn get_by_external_id(
        &self,
        mikan_id: Option<&str>,
        bgmtv_id: Option<i64>,
        tmdb_id: Option<i64>,
    ) -> Result<Option<Metadata>, MetadataError> {
        // Try mikan_id first
        if let Some(mikan_id) = mikan_id {
            if let Some(metadata) = MetadataRepository::get_by_mikan_id(&self.db, mikan_id).await? {
                return Ok(Some(metadata));
            }
        }

        // Try bgmtv_id
        if let Some(bgmtv_id) = bgmtv_id {
            if let Some(metadata) = MetadataRepository::get_by_bgmtv_id(&self.db, bgmtv_id).await? {
                return Ok(Some(metadata));
            }
        }

        // Try tmdb_id
        if let Some(tmdb_id) = tmdb_id {
            if let Some(metadata) = MetadataRepository::get_by_tmdb_id(&self.db, tmdb_id).await? {
                return Ok(Some(metadata));
            }
        }

        Ok(None)
    }

    /// Find existing metadata by external ID or create new
    pub async fn find_or_create(&self, data: CreateMetadata) -> Result<Metadata, MetadataError> {
        // Try to find existing metadata by external IDs
        if let Some(existing) = self
            .get_by_external_id(
                data.mikan_id.as_deref(),
                data.bgmtv_id,
                data.tmdb_id,
            )
            .await?
        {
            return Ok(existing);
        }

        // Create new metadata
        self.create(data).await
    }

    /// Update metadata
    pub async fn update(&self, id: i64, data: UpdateMetadata) -> Result<Metadata, MetadataError> {
        MetadataRepository::update(&self.db, id, data)
            .await?
            .ok_or(MetadataError::NotFound)
    }

    /// Update poster URL
    pub async fn update_poster_url(&self, id: i64, poster_url: &str) -> Result<bool, MetadataError> {
        Ok(MetadataRepository::update_poster_url(&self.db, id, poster_url).await?)
    }

    /// Get all metadata
    pub async fn get_all(&self) -> Result<Vec<Metadata>, MetadataError> {
        Ok(MetadataRepository::get_all(&self.db).await?)
    }

    /// Delete metadata by ID
    pub async fn delete(&self, id: i64) -> Result<bool, MetadataError> {
        Ok(MetadataRepository::delete(&self.db, id).await?)
    }

    /// Get database pool reference (for repositories that need direct access)
    pub fn pool(&self) -> &SqlitePool {
        &self.db
    }
}
