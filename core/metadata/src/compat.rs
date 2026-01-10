//! Compatibility layer for existing code
//!
//! This module provides the old MetadataProvider trait and adapters
//! for backward compatibility with existing domain and server code.

mod bgmtv_adapter;
mod provider;

pub use bgmtv_adapter::{
    BgmtvProvider, Episode, EpisodeType, Platform, SearchQuery, SearchedMetadata,
};
pub use provider::{MetadataProvider, ProviderError};

/// Metadata source identifier (compat)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "openapi", derive(utoipa::ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum MetadataSource {
    Bgmtv,
    Tmdb,
}

// Re-export TmdbProvider as a stub for compatibility
pub struct TmdbProvider {
    #[allow(dead_code)]
    client: std::sync::Arc<tmdb::TmdbClient>,
}

impl TmdbProvider {
    pub fn new(client: std::sync::Arc<tmdb::TmdbClient>) -> Self {
        Self { client }
    }
}

#[async_trait::async_trait]
impl MetadataProvider for TmdbProvider {
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError> {
        // Use discover_bangumi for anime search
        let params = tmdb::DiscoverBangumiParams {
            with_text_query: Some(clean_title_for_search(&query.keyword)),
        };

        let response = self.client.discover_bangumi(params).await?;

        Ok(response
            .results
            .into_iter()
            .map(|show| {
                let year = show
                    .first_air_date
                    .as_ref()
                    .and_then(|date| date.split('-').next())
                    .and_then(|y| y.parse::<i32>().ok());

                SearchedMetadata {
                    source: MetadataSource::Tmdb,
                    external_id: show.id.to_string(),
                    title_chinese: Some(show.name),
                    title_original: Some(show.original_name),
                    year,
                    season: None,
                    platform: None,
                    total_episodes: 0,
                    poster_url: show
                        .poster_path
                        .map(|path| format!("https://image.tmdb.org/t/p/w500{}", path)),
                    air_date: show.first_air_date,
                }
            })
            .collect())
    }

    fn name(&self) -> &'static str {
        "tmdb"
    }
}

/// Clean title for TMDB search
fn clean_title_for_search(title: &str) -> String {
    use regex::Regex;
    use std::sync::LazyLock;

    static SPLIT_COUR_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"(?i)(?:第\s*\d+\s*(?:クール|部分))|(?:Part\s*\d+)").unwrap()
    });

    static SEASON_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(
            r"(?i)(?:第[0-9一二三四五六七八九十]+(?:季|期))|(?:SEASON\s*\d{1,2})|(?:S\d{1,2})",
        )
        .unwrap()
    });

    let mut result = title.to_string();
    result = SPLIT_COUR_PATTERN.replace_all(&result, "").to_string();
    result = SEASON_PATTERN.replace_all(&result, "").to_string();
    result.split_whitespace().collect::<Vec<_>>().join(" ")
}
