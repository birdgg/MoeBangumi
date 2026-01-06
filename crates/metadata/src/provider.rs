//! Metadata provider trait definition

use async_trait::async_trait;

use crate::{ProviderError, SearchQuery, SearchedMetadata};

/// Unified metadata provider trait
///
/// This trait defines a standard interface for searching metadata from
/// different data sources (BGM.tv, TMDB).
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

    /// Calculate episode offset for a subject.
    ///
    /// Episode offset converts absolute episode numbers (from RSS feeds)
    /// to season-relative episode numbers. For example:
    /// - Season 2 starts at episode 13 (absolute) but should be episode 1 (relative)
    /// - offset = 13 - 1 = 12
    /// - When RSS gives episode 15, adjusted = 15 - 12 = 3 (S02E03)
    ///
    /// # Arguments
    /// * `external_id` - The external ID of the subject (e.g., BGM.tv subject ID)
    ///
    /// # Returns
    /// * `Ok(offset)` - The calculated episode offset
    /// * Default implementation returns 0 (no offset)
    async fn get_episode_offset(&self, _external_id: &str) -> Result<i32, ProviderError> {
        Ok(0)
    }

    /// Provider name for logging and debugging
    fn name(&self) -> &'static str;
}
