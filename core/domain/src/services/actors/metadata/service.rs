use std::sync::Arc;

use metadata::{
    BgmtvProvider, Episode, MetadataProvider, MetadataSource, SearchQuery, SearchedMetadata,
    TmdbProvider,
};
use sqlx::SqlitePool;

use super::error::MetadataError;
use crate::models::{CreateMetadata, Metadata, Platform, UpdateMetadata};
use crate::repositories::MetadataRepository;

/// Fetched metadata from BGM.tv (not persisted yet)
#[derive(Debug, Clone)]
pub struct FetchedMetadata {
    pub bgmtv_id: i64,
    pub title_chinese: Option<String>,
    pub title_japanese: Option<String>,
    pub year: Option<i32>,
    pub season: Option<i32>,
    pub total_episodes: i32,
    pub poster_url: Option<String>,
    pub air_date: Option<String>,
    pub platform: Option<Platform>,
}

/// Service for managing Metadata entities
pub struct MetadataService {
    db: SqlitePool,
    bgmtv_provider: Arc<BgmtvProvider>,
    tmdb_provider: Arc<TmdbProvider>,
}

impl MetadataService {
    /// Create a new MetadataService
    pub fn new(
        db: SqlitePool,
        bgmtv_provider: Arc<BgmtvProvider>,
        tmdb_provider: Arc<TmdbProvider>,
    ) -> Self {
        Self {
            db,
            bgmtv_provider,
            tmdb_provider,
        }
    }

    /// Fetch metadata from BGM.tv by ID (does not persist)
    pub async fn fetch_from_bgmtv(&self, id: i64) -> Result<FetchedMetadata, MetadataError> {
        let result = self
            .bgmtv_provider
            .get_detail(&id.to_string())
            .await?
            .ok_or_else(|| MetadataError::NotFound(id))?;

        Ok(FetchedMetadata {
            bgmtv_id: result.external_id.parse().unwrap_or(0),
            title_chinese: result.title_chinese,
            title_japanese: result.title_original,
            year: result.year,
            season: result.season,
            total_episodes: result.total_episodes,
            poster_url: result.poster_url,
            air_date: result.air_date,
            platform: result.platform.map(|p| match p {
                metadata::Platform::Tv => Platform::Tv,
                metadata::Platform::Movie => Platform::Movie,
                metadata::Platform::Ova => Platform::Ova,
            }),
        })
    }

    /// Search for TMDB ID by title with optional year filtering
    ///
    /// Uses TmdbProvider to find the best matching TMDB ID.
    pub async fn find_tmdb_id(
        &self,
        title: &str,
        year: Option<i32>,
    ) -> Result<Option<i64>, MetadataError> {
        let mut query = SearchQuery::new(title);
        if let Some(y) = year {
            query = query.with_year(y);
        }

        let result = self.tmdb_provider.find(&query).await?;
        Ok(result.and_then(|r| r.external_id.parse().ok()))
    }

    /// Create new metadata
    pub async fn create(&self, data: CreateMetadata) -> Result<Metadata, MetadataError> {
        Ok(MetadataRepository::create(&self.db, data).await?)
    }

    /// Get metadata by ID
    pub async fn get_by_id(&self, id: i64) -> Result<Metadata, MetadataError> {
        MetadataRepository::get_by_id(&self.db, id)
            .await?
            .ok_or(MetadataError::NotFound(id))
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

    /// Find existing metadata by external ID and update it, or create new if not found.
    ///
    /// This method implements a "merge update" strategy:
    /// - If metadata with matching external ID (mikan_id, bgmtv_id, or tmdb_id) exists,
    ///   update it with the new data while preserving unspecified optional fields.
    /// - If no matching metadata exists, create a new record.
    ///
    /// # Merge Behavior
    /// When updating existing metadata, only fields provided in `CreateMetadata` are updated.
    /// Optional fields that are `None` in the input will preserve their existing values.
    /// This allows partial updates when creating Bangumi with incomplete metadata.
    ///
    /// # Note
    /// If multiple Bangumi share the same metadata (via metadata_id), updating through
    /// this method will affect all of them. This is intentional as metadata represents
    /// the canonical information about an anime.
    pub async fn find_or_update(&self, data: CreateMetadata) -> Result<Metadata, MetadataError> {
        // Try to find existing metadata by external IDs
        if let Some(existing) = self
            .get_by_external_id(data.mikan_id.as_deref(), data.bgmtv_id, data.tmdb_id)
            .await?
        {
            // Convert CreateMetadata to UpdateMetadata and merge
            let update_data = data.into_update();
            return self.update(existing.id, update_data).await;
        }

        // Create new metadata
        self.create(data).await
    }

    /// Update metadata
    pub async fn update(&self, id: i64, data: UpdateMetadata) -> Result<Metadata, MetadataError> {
        MetadataRepository::update(&self.db, id, data)
            .await?
            .ok_or(MetadataError::NotFound(id))
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

    // ============ Unified Provider Interface ============

    /// Get metadata detail by external ID from a specific source
    pub async fn get_provider_detail(
        &self,
        external_id: &str,
        source: MetadataSource,
    ) -> Result<Option<SearchedMetadata>, MetadataError> {
        let result = match source {
            MetadataSource::Bgmtv => self.bgmtv_provider.get_detail(external_id).await?,
            MetadataSource::Tmdb => self.tmdb_provider.get_detail(external_id).await?,
        };
        Ok(result)
    }

    /// Search metadata from a specific source
    pub async fn search_provider(
        &self,
        query: &SearchQuery,
        source: MetadataSource,
    ) -> Result<Vec<SearchedMetadata>, MetadataError> {
        let results = match source {
            MetadataSource::Bgmtv => self.bgmtv_provider.search(query).await?,
            MetadataSource::Tmdb => self.tmdb_provider.search(query).await?,
        };
        Ok(results)
    }

    /// Find best matching metadata from a specific source
    pub async fn find_provider(
        &self,
        query: &SearchQuery,
        source: MetadataSource,
    ) -> Result<Option<SearchedMetadata>, MetadataError> {
        let result = match source {
            MetadataSource::Bgmtv => self.bgmtv_provider.find(query).await?,
            MetadataSource::Tmdb => self.tmdb_provider.find(query).await?,
        };
        Ok(result)
    }

    /// Get episodes from BGM.tv (only BGM.tv supports episodes)
    pub async fn get_episodes(&self, bgmtv_id: i64) -> Result<Vec<Episode>, MetadataError> {
        let episodes = self
            .bgmtv_provider
            .get_episodes(&bgmtv_id.to_string())
            .await?;
        Ok(episodes)
    }
}
