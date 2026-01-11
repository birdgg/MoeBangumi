//! Unified metadata client combining BGM.tv and TMDB providers.

use crate::{
    BgmtvProvider, CombinedSearchResults, Episode, MetadataProvider, MetadataSource, ProviderError,
    SearchQuery, SearchedMetadata, TmdbProvider,
};

/// Unified metadata client that wraps BGM.tv and TMDB providers.
///
/// This client provides a simple interface to search and retrieve metadata
/// from different sources based on the `MetadataSource` parameter.
///
/// # Example
///
/// ```ignore
/// use metadata::{MetadataClient, MetadataSource, SearchQuery};
///
/// let client = MetadataClient::new(bgmtv_provider, tmdb_provider);
///
/// // Search from BGM.tv
/// let query = SearchQuery::new("葬送のフリーレン");
/// let results = client.search(&query, MetadataSource::Bgmtv).await?;
///
/// // Get detail from TMDB
/// let detail = client.get_detail("12345", MetadataSource::Tmdb).await?;
///
/// // Get episode offset (BGM.tv only)
/// let offset = client.get_offset("425651").await?;
/// ```
pub struct MetadataClient {
    bgmtv: BgmtvProvider,
    tmdb: TmdbProvider,
}

impl MetadataClient {
    /// Create a new MetadataClient with BGM.tv and TMDB providers.
    pub fn new(bgmtv: BgmtvProvider, tmdb: TmdbProvider) -> Self {
        Self { bgmtv, tmdb }
    }

    /// Search for metadata from the specified source.
    ///
    /// Returns multiple results sorted by relevance.
    pub async fn search(
        &self,
        query: &SearchQuery,
        source: MetadataSource,
    ) -> Result<Vec<SearchedMetadata>, ProviderError> {
        match source {
            MetadataSource::Bgmtv => self.bgmtv.search(query).await,
            MetadataSource::Tmdb => self.tmdb.search(query).await,
        }
    }

    /// Get detailed metadata by external ID from the specified source.
    ///
    /// Returns full metadata for a specific subject/show.
    pub async fn get_detail(
        &self,
        external_id: &str,
        source: MetadataSource,
    ) -> Result<Option<SearchedMetadata>, ProviderError> {
        match source {
            MetadataSource::Bgmtv => self.bgmtv.get_detail(external_id).await,
            MetadataSource::Tmdb => self.tmdb.get_detail(external_id).await,
        }
    }

    /// Get episode offset for a BGM.tv subject.
    ///
    /// Episode offset converts absolute episode numbers (from RSS feeds)
    /// to season-relative episode numbers. For example:
    /// - Season 2 starts at episode 13 (absolute) but should be episode 1 (relative)
    /// - offset = 13 - 1 = 12
    /// - When RSS gives episode 15, adjusted = 15 - 12 = 3 (S02E03)
    ///
    /// # Arguments
    /// * `bgmtv_id` - The BGM.tv subject ID
    ///
    /// # Note
    /// This method only works with BGM.tv IDs, as only BGM.tv provides
    /// episode data needed to calculate the offset.
    pub async fn get_offset(&self, bgmtv_id: &str) -> Result<i32, ProviderError> {
        self.bgmtv.get_episode_offset(bgmtv_id).await
    }

    /// Find the best matching metadata from the specified source.
    ///
    /// When `year` is provided in the query, filters results to match
    /// shows that aired within ±1 year of the expected year.
    ///
    /// Returns the first matching result, or None if no matches found.
    pub async fn find(
        &self,
        query: &SearchQuery,
        source: MetadataSource,
    ) -> Result<Option<SearchedMetadata>, ProviderError> {
        match source {
            MetadataSource::Bgmtv => self.bgmtv.find(query).await,
            MetadataSource::Tmdb => self.tmdb.find(query).await,
        }
    }

    /// Get episodes for a subject from the specified source.
    ///
    /// Currently only BGM.tv provides episode data.
    /// TMDB returns an empty list.
    pub async fn get_episodes(
        &self,
        external_id: &str,
        source: MetadataSource,
    ) -> Result<Vec<Episode>, ProviderError> {
        match source {
            MetadataSource::Bgmtv => self.bgmtv.get_episodes(external_id).await,
            MetadataSource::Tmdb => self.tmdb.get_episodes(external_id).await,
        }
    }

    /// Search for metadata from all sources in parallel.
    ///
    /// Returns grouped results from BGM.tv and TMDB. If one source fails,
    /// the other source's results are still returned (with a warning logged).
    ///
    /// # Example
    ///
    /// ```ignore
    /// let results = client.search_all(&query).await;
    /// println!("BGM.tv: {} results", results.bgmtv.len());
    /// println!("TMDB: {} results", results.tmdb.len());
    /// ```
    pub async fn search_all(&self, query: &SearchQuery) -> CombinedSearchResults {
        let (bgmtv_result, tmdb_result) =
            tokio::join!(self.bgmtv.search(query), self.tmdb.search(query),);

        CombinedSearchResults {
            bgmtv: bgmtv_result.unwrap_or_else(|e| {
                tracing::warn!("BGM.tv search failed for '{}': {}", query.keyword, e);
                vec![]
            }),
            tmdb: tmdb_result.unwrap_or_else(|e| {
                tracing::warn!("TMDB search failed for '{}': {}", query.keyword, e);
                vec![]
            }),
        }
    }
}
