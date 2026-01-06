//! Metadata provider trait definition

use async_trait::async_trait;

use crate::{ProviderError, SearchQuery, SearchedMetadata};

/// Unified metadata provider trait
///
/// This trait defines a standard interface for searching metadata from
/// different data sources (BGM.tv, TMDB, Mikan).
#[async_trait]
pub trait MetadataProvider: Send + Sync {
    /// Search for metadata matching the query
    ///
    /// Returns multiple results sorted by relevance.
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError>;

    /// Find the best matching metadata for the query
    ///
    /// When `year` is provided in the query, filters results to match
    /// shows that aired within ±1 year of the expected year.
    ///
    /// Returns the first matching result, or None if no matches found.
    async fn find(&self, query: &SearchQuery) -> Result<Option<SearchedMetadata>, ProviderError> {
        let results = self.search(query).await?;

        if results.is_empty() {
            return Ok(None);
        }

        // If year filter is provided, find the first year-matching result
        if let Some(expected_year) = query.year {
            for result in &results {
                if result.matches_year(expected_year) {
                    return Ok(Some(result.clone()));
                }
            }
            // No year match found, fall back to first result
            tracing::debug!(
                "No year-matched result for '{}' (expected {}), using first result",
                query.keyword,
                expected_year
            );
        }

        Ok(results.into_iter().next())
    }

    /// Provider name for logging and debugging
    fn name(&self) -> &'static str;
}
