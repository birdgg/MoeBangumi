//! Metadata provider trait definition (compatibility)

use async_trait::async_trait;

use super::{Episode, SearchQuery, SearchedMetadata};

/// Errors that can occur when using metadata providers
#[derive(Debug, thiserror::Error)]
pub enum ProviderError {
    #[error("Data source {0:?} is not available")]
    SourceNotAvailable(super::MetadataSource),

    #[error("BGM.tv error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),

    #[error("TMDB error: {0}")]
    Tmdb(#[from] tmdb::TmdbError),
}

/// Unified metadata provider trait (compatibility)
#[async_trait]
pub trait MetadataProvider: Send + Sync {
    /// Search for metadata matching the query
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError>;

    /// Find the best matching metadata for the query
    async fn find(&self, query: &SearchQuery) -> Result<Option<SearchedMetadata>, ProviderError> {
        let results = self.search(query).await?;

        if results.is_empty() {
            return Ok(None);
        }

        if let Some(expected_year) = query.year {
            for result in &results {
                if result.matches_year(expected_year) {
                    return Ok(Some(result.clone()));
                }
            }
        }

        Ok(results.into_iter().next())
    }

    /// Get detailed metadata by external ID
    async fn get_detail(
        &self,
        _external_id: &str,
    ) -> Result<Option<SearchedMetadata>, ProviderError> {
        Ok(None)
    }

    /// Get episodes for a subject by external ID
    async fn get_episodes(&self, _external_id: &str) -> Result<Vec<Episode>, ProviderError> {
        Ok(vec![])
    }

    /// Calculate episode offset for a subject
    async fn get_episode_offset(&self, _external_id: &str) -> Result<i32, ProviderError> {
        Ok(0)
    }

    /// Provider name for logging and debugging
    fn name(&self) -> &'static str;
}
