//! Torrent metadata resolver service
//!
//! This service resolves metadata for torrents that are not tracked in the database.
//! It parses torrent names to extract search information and searches metadata
//! from BGM.tv (priority) and TMDB.

use std::sync::Arc;

use chrono::Datelike;
use metadata::{MetadataSource, SearchQuery};
use parser::Parser;
use sqlx::SqlitePool;

use crate::models::{Bangumi, CreateMetadata, Metadata, Platform, SourceType};
use crate::repositories::{BangumiRepository, CreateBangumiData};
use crate::services::actors::metadata::{MetadataError, MetadataService};
use crate::services::SettingsService;

/// Error type for torrent metadata resolution
#[derive(Debug, thiserror::Error)]
pub enum ResolverError {
    #[error("Failed to extract search info from torrent: {0}")]
    ExtractionFailed(String),

    #[error("No metadata found for query: {0}")]
    MetadataNotFound(String),

    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Metadata service error: {0}")]
    Metadata(#[from] MetadataError),

    #[error("Path generation failed: {0}")]
    PathGeneration(String),
}

/// Result of metadata resolution
#[derive(Debug)]
pub struct ResolvedMetadata {
    /// The bangumi record
    pub bangumi: Bangumi,
    /// The associated metadata
    pub metadata: Metadata,
    /// Whether this is a newly created bangumi
    pub is_new: bool,
}

/// Information extracted from torrent for searching
#[derive(Debug, Clone)]
pub struct ExtractedSearchInfo {
    /// Chinese title (preferred for search)
    pub title_chinese: Option<String>,
    /// Japanese title
    pub title_japanese: Option<String>,
    /// English title
    pub title_english: Option<String>,
    /// Detected season number
    pub season: Option<i32>,
    /// Year (if detectable from torrent name)
    pub year: Option<i32>,
}

impl ExtractedSearchInfo {
    /// Get the best title for searching (prefer Chinese > Japanese > English)
    pub fn best_search_title(&self) -> Option<&str> {
        self.title_chinese
            .as_deref()
            .or(self.title_japanese.as_deref())
            .or(self.title_english.as_deref())
    }
}

/// Service for resolving metadata from torrent information
pub struct TorrentMetadataResolver {
    db: SqlitePool,
    metadata_service: Arc<MetadataService>,
    settings_service: Arc<SettingsService>,
    parser: Parser,
}

impl TorrentMetadataResolver {
    /// Create a new TorrentMetadataResolver
    pub fn new(
        db: SqlitePool,
        metadata_service: Arc<MetadataService>,
        settings_service: Arc<SettingsService>,
    ) -> Self {
        Self {
            db,
            metadata_service,
            settings_service,
            parser: Parser::new(),
        }
    }

    /// Resolve metadata for a torrent task
    ///
    /// This method:
    /// 1. Extracts search info from the task name
    /// 2. Searches metadata from BGM.tv (priority) and TMDB
    /// 3. Creates or finds existing bangumi with the metadata
    ///
    /// Returns `ResolvedMetadata` containing bangumi and metadata.
    pub async fn resolve(
        &self,
        task_name: &str,
    ) -> Result<ResolvedMetadata, ResolverError> {
        // Extract search info from task name
        let search_info = self.extract_search_info(task_name)?;

        let title = search_info.best_search_title().ok_or_else(|| {
            ResolverError::ExtractionFailed(format!(
                "No title found in task name: {}",
                task_name
            ))
        })?;

        tracing::info!(
            "Extracted title '{}' from task '{}', searching metadata...",
            title,
            task_name
        );

        // Search metadata (BGM.tv first, then TMDB)
        let (searched_metadata, source) = self.search_metadata(title, search_info.year).await?;

        tracing::info!(
            "Found metadata from {:?}: {} (external_id: {})",
            source,
            searched_metadata
                .title_chinese
                .as_deref()
                .unwrap_or("Unknown"),
            searched_metadata.external_id
        );

        // Create or find bangumi with metadata
        self.create_or_find_bangumi(searched_metadata, source, search_info.season)
            .await
    }

    /// Resolve metadata for fallback (when search fails)
    ///
    /// Creates a minimal metadata record using parsed title.
    pub async fn resolve_fallback(
        &self,
        task_name: &str,
    ) -> Result<(ExtractedSearchInfo, Metadata), ResolverError> {
        let search_info = self.extract_search_info(task_name)?;

        let title = search_info.best_search_title().ok_or_else(|| {
            ResolverError::ExtractionFailed(format!(
                "No title found in task name: {}",
                task_name
            ))
        })?;

        // Create minimal metadata for fallback
        let create_metadata = CreateMetadata {
            mikan_id: None,
            bgmtv_id: None,
            tmdb_id: None,
            title_chinese: title.to_string(),
            title_japanese: search_info.title_japanese.clone(),
            season: search_info.season.unwrap_or(1),
            year: chrono::Utc::now().year(),
            platform: Platform::Tv,
            total_episodes: 0,
            poster_url: None,
            air_date: None,
            air_week: 0,
            episode_offset: 0,
        };

        let metadata = self.metadata_service.find_or_update(create_metadata).await?;

        Ok((search_info, metadata))
    }

    /// Extract search information from torrent task name
    fn extract_search_info(&self, task_name: &str) -> Result<ExtractedSearchInfo, ResolverError> {
        // Parse the task name
        let parse_result = self.parser.parse(task_name).map_err(|e| {
            ResolverError::ExtractionFailed(format!(
                "Failed to parse task name '{}': {}",
                task_name, e
            ))
        })?;

        // Clean up titles by removing season info for better search
        let title_chinese = parse_result.name_zh.map(|t| Self::clean_title_for_search(&t));
        let title_japanese = parse_result.name_jp;
        let title_english = parse_result.name_en;

        Ok(ExtractedSearchInfo {
            title_chinese,
            title_japanese,
            title_english,
            season: parse_result.season,
            year: None, // Will be inferred from search results
        })
    }

    /// Clean title for search by removing common suffixes
    fn clean_title_for_search(title: &str) -> String {
        // Remove common season patterns for better search
        let cleaned = title
            .replace("第一季", "")
            .replace("第二季", "")
            .replace("第三季", "")
            .replace("第四季", "")
            .replace("第五季", "")
            .replace("Season 1", "")
            .replace("Season 2", "")
            .replace("Season 3", "")
            .replace("S1", "")
            .replace("S2", "")
            .replace("S3", "")
            .trim()
            .to_string();

        if cleaned.is_empty() {
            title.to_string()
        } else {
            cleaned
        }
    }

    /// Search metadata from providers (BGM.tv priority, then TMDB)
    async fn search_metadata(
        &self,
        title: &str,
        year: Option<i32>,
    ) -> Result<(metadata::SearchedMetadata, MetadataSource), ResolverError> {
        let mut query = SearchQuery::new(title);
        if let Some(y) = year {
            query = query.with_year(y);
        }

        // Try BGM.tv first (anime-specific)
        match self.metadata_service.find_provider(&query, MetadataSource::Bgmtv).await {
            Ok(Some(result)) => {
                tracing::debug!(
                    "Found match in BGM.tv: {}",
                    result.title_chinese.as_deref().unwrap_or(&result.external_id)
                );
                return Ok((result, MetadataSource::Bgmtv));
            }
            Ok(None) => {
                tracing::debug!("No match found in BGM.tv for: {}", title);
            }
            Err(e) => {
                tracing::warn!("BGM.tv search failed for '{}': {}", title, e);
            }
        }

        // Fallback to TMDB
        let settings = self.settings_service.get();
        if settings.tmdb.is_configured() {
            match self.metadata_service.find_provider(&query, MetadataSource::Tmdb).await {
                Ok(Some(result)) => {
                    tracing::debug!(
                        "Found match in TMDB: {}",
                        result.title_chinese.as_deref().unwrap_or(&result.external_id)
                    );
                    return Ok((result, MetadataSource::Tmdb));
                }
                Ok(None) => {
                    tracing::debug!("No match found in TMDB for: {}", title);
                }
                Err(e) => {
                    tracing::warn!("TMDB search failed for '{}': {}", title, e);
                }
            }
        }

        Err(ResolverError::MetadataNotFound(title.to_string()))
    }

    /// Create or find existing bangumi with the searched metadata
    async fn create_or_find_bangumi(
        &self,
        searched: metadata::SearchedMetadata,
        source: MetadataSource,
        parsed_season: Option<i32>,
    ) -> Result<ResolvedMetadata, ResolverError> {
        // Convert SearchedMetadata to CreateMetadata
        let create_metadata = CreateMetadata {
            mikan_id: None,
            bgmtv_id: match source {
                MetadataSource::Bgmtv => searched.external_id.parse().ok(),
                MetadataSource::Tmdb => None,
            },
            tmdb_id: match source {
                MetadataSource::Tmdb => searched.external_id.parse().ok(),
                MetadataSource::Bgmtv => None,
            },
            title_chinese: searched
                .title_chinese
                .unwrap_or_else(|| searched.title_original.clone().unwrap_or_default()),
            title_japanese: searched.title_original,
            season: parsed_season.unwrap_or_else(|| searched.season.unwrap_or(1)),
            year: searched.year.unwrap_or_else(|| chrono::Utc::now().year()),
            platform: searched.platform.unwrap_or(Platform::Tv),
            total_episodes: searched.total_episodes,
            poster_url: searched.poster_url,
            air_date: searched.air_date,
            air_week: 0,
            episode_offset: 0,
        };

        // Find or create metadata
        let metadata = self.metadata_service.find_or_update(create_metadata).await?;

        // Check if bangumi already exists for this metadata
        if let Some(existing) = BangumiRepository::get_by_metadata_id(&self.db, metadata.id).await?
        {
            tracing::info!(
                "Using existing Bangumi {} for metadata {}",
                existing.id,
                metadata.id
            );
            return Ok(ResolvedMetadata {
                bangumi: existing,
                metadata,
                is_new: false,
            });
        }

        // Generate save path
        let settings = self.settings_service.get();
        let base_path = &settings.downloader.save_path;
        let save_path = pathgen::generate_directory(
            base_path,
            &metadata.title_chinese,
            metadata.year,
            metadata.season,
            metadata.tmdb_id,
            Some(metadata.platform.as_str()),
        )
        .map_err(|e| ResolverError::PathGeneration(e.to_string()))?;

        // Create new bangumi
        tracing::info!(
            "Creating new Bangumi for metadata {} at {}",
            metadata.id,
            save_path
        );

        let create_data = CreateBangumiData {
            metadata_id: metadata.id,
            auto_complete: true,
            save_path,
            source_type: SourceType::WebRip.as_str().to_string(),
            current_episode: None,
        };

        let bangumi = BangumiRepository::create(&self.db, create_data).await?;

        Ok(ResolvedMetadata {
            bangumi,
            metadata,
            is_new: true,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clean_title_for_search() {
        assert_eq!(
            TorrentMetadataResolver::clean_title_for_search("我推的孩子 第二季"),
            "我推的孩子"
        );
        assert_eq!(
            TorrentMetadataResolver::clean_title_for_search("葬送的芙莉莲 Season 2"),
            "葬送的芙莉莲"
        );
        assert_eq!(
            TorrentMetadataResolver::clean_title_for_search("迷宫饭"),
            "迷宫饭"
        );
    }

    #[test]
    fn test_extracted_search_info_best_title() {
        let info = ExtractedSearchInfo {
            title_chinese: Some("中文标题".to_string()),
            title_japanese: Some("日本語".to_string()),
            title_english: Some("English".to_string()),
            season: None,
            year: None,
        };
        assert_eq!(info.best_search_title(), Some("中文标题"));

        let info_no_chinese = ExtractedSearchInfo {
            title_chinese: None,
            title_japanese: Some("日本語".to_string()),
            title_english: Some("English".to_string()),
            season: None,
            year: None,
        };
        assert_eq!(info_no_chinese.best_search_title(), Some("日本語"));
    }
}
