use std::sync::Arc;

use bgmtv::BgmtvClient;
use sqlx::SqlitePool;
use thiserror::Error;
use tmdb::{DiscoverBangumiParams, TmdbClient};

use crate::models::{CreateMetadata, Metadata, Platform, UpdateMetadata};
use crate::repositories::MetadataRepository;

#[derive(Debug, Error)]
pub enum MetadataError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Metadata not found")]
    NotFound,
    #[error("BGM.tv API error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),
    #[error("TMDB API error: {0}")]
    Tmdb(#[from] tmdb::TmdbError),
}

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
    bgmtv: Arc<BgmtvClient>,
    tmdb: Arc<TmdbClient>,
}

impl MetadataService {
    /// Create a new MetadataService
    pub fn new(db: SqlitePool, bgmtv: Arc<BgmtvClient>, tmdb: Arc<TmdbClient>) -> Self {
        Self { db, bgmtv, tmdb }
    }

    /// Fetch metadata from BGM.tv by ID (does not persist)
    pub async fn fetch_from_bgmtv(&self, id: i64) -> Result<FetchedMetadata, MetadataError> {
        // get_subject now returns ParsedSubject directly
        let parsed = self.bgmtv.get_subject(id).await?;

        Ok(FetchedMetadata {
            bgmtv_id: parsed.bgmtv_id,
            title_chinese: parsed.title_chinese,
            title_japanese: parsed.title_japanese,
            year: parsed.year,
            season: Some(parsed.season),
            total_episodes: parsed.total_episodes as i32,
            poster_url: Some(parsed.poster_url),
            air_date: parsed.air_date,
            platform: parse_platform(&parsed.platform),
        })
    }

    /// Search for TMDB ID by title
    pub async fn find_tmdb_id(&self, title: &str) -> Result<Option<i64>, MetadataError> {
        let params = DiscoverBangumiParams {
            with_text_query: Some(title.to_string()),
        };
        let response = self.tmdb.discover_bangumi(params).await?;
        Ok(response.results.into_iter().next().map(|show| show.id))
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

    /// Check if an anime has finished airing via BGM.tv episodes API
    ///
    /// Returns `Ok(Some(true))` if finished, `Ok(Some(false))` if still airing,
    /// `Ok(None)` if cannot determine (no episodes or airdate), or `Err` on failure.
    pub async fn check_finished_status(&self, bgmtv_id: i64) -> Result<Option<bool>, MetadataError> {
        use chrono::Utc;

        // Fetch all episodes
        let response = self.bgmtv.get_episodes(bgmtv_id).await?;

        // Find last main episode (type = 0)
        let last_main_episode = response
            .data
            .iter()
            .filter(|ep| matches!(ep.episode_type, bgmtv::EpisodeType::Main))
            .max_by(|a, b| {
                match (a.ep, b.ep) {
                    (Some(a_ep), Some(b_ep)) => {
                        a_ep.partial_cmp(&b_ep).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    _ => a.sort.partial_cmp(&b.sort).unwrap_or(std::cmp::Ordering::Equal),
                }
            });

        // No main episodes = cannot determine
        let Some(last_episode) = last_main_episode else {
            return Ok(None);
        };

        // No airdate = cannot determine
        if last_episode.airdate.is_empty() {
            return Ok(None);
        }

        // Parse and compare with today
        let airdate = match chrono::NaiveDate::parse_from_str(&last_episode.airdate, "%Y-%m-%d") {
            Ok(date) => date,
            Err(e) => {
                tracing::warn!(
                    "Failed to parse airdate '{}' for bgmtv_id {}: {}",
                    last_episode.airdate,
                    bgmtv_id,
                    e
                );
                return Ok(None);
            }
        };

        let today = Utc::now().date_naive();

        Ok(Some(airdate <= today))
    }

    /// Batch check and update finish status for all unfinished metadata
    ///
    /// Returns (updated_count, total_checked, error_count)
    pub async fn batch_check_finish_status(&self) -> Result<(usize, usize, usize), MetadataError> {
        use std::time::Duration;

        // Get all unfinished metadata with bgmtv_id
        let metadata_list = MetadataRepository::get_unfinished_with_bgmtv_id(&self.db).await?;

        let total = metadata_list.len();
        let mut updated_count = 0;
        let mut error_count = 0;

        for metadata in metadata_list {
            let bgmtv_id = match metadata.bgmtv_id {
                Some(id) => id,
                None => continue,
            };

            // Check finish status
            match self.check_finished_status(bgmtv_id).await {
                Ok(Some(true)) => {
                    // Mark as finished
                    match MetadataRepository::mark_as_finished(&self.db, metadata.id).await {
                        Ok(true) => {
                            updated_count += 1;
                            tracing::info!(
                                "Marked metadata {} (bgmtv_id={}, title='{}') as finished",
                                metadata.id,
                                bgmtv_id,
                                metadata.title_chinese
                            );
                        }
                        Ok(false) => {
                            // Record may have been deleted during batch processing, skip silently
                            tracing::debug!(
                                "Metadata {} (bgmtv_id={}, title='{}') not found during update, skipping",
                                metadata.id,
                                bgmtv_id,
                                metadata.title_chinese
                            );
                        }
                        Err(e) => {
                            error_count += 1;
                            tracing::error!(
                                "Failed to update metadata {} (bgmtv_id={}, title='{}'): {}",
                                metadata.id,
                                bgmtv_id,
                                metadata.title_chinese,
                                e
                            );
                        }
                    }
                }
                Ok(Some(false)) => {
                    // Still airing
                    tracing::debug!(
                        "Metadata {} (bgmtv_id={}, title='{}') is still airing",
                        metadata.id,
                        bgmtv_id,
                        metadata.title_chinese
                    );
                }
                Ok(None) => {
                    // Cannot determine (no episodes or airdate)
                    tracing::debug!(
                        "Cannot determine finish status for metadata {} (bgmtv_id={}, title='{}')",
                        metadata.id,
                        bgmtv_id,
                        metadata.title_chinese
                    );
                }
                Err(e) => {
                    error_count += 1;
                    tracing::error!(
                        "Failed to check metadata {} (bgmtv_id={}, title='{}'): {}",
                        metadata.id,
                        bgmtv_id,
                        metadata.title_chinese,
                        e
                    );
                }
            }

            // Rate limiting: avoid hitting BGM.tv API rate limits
            tokio::time::sleep(Duration::from_millis(200)).await;
        }

        Ok((updated_count, total, error_count))
    }
}

fn parse_platform(platform: &str) -> Option<Platform> {
    match platform.to_lowercase().as_str() {
        "tv" => Some(Platform::Tv),
        "movie" | "劇場版" => Some(Platform::Movie),
        "ova" => Some(Platform::Ova),
        _ => None,
    }
}
