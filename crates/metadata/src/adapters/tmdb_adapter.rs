//! TMDB metadata provider adapter

use std::sync::Arc;

use async_trait::async_trait;
use tmdb::{DiscoverBangumiParams, TmdbClient};

use crate::{MetadataProvider, MetadataSource, ProviderError, SearchQuery, SearchedMetadata};

/// TMDB image base URL
const TMDB_IMAGE_BASE_URL: &str = "https://image.tmdb.org/t/p/w500";

/// TMDB metadata provider
pub struct TmdbProvider {
    client: Arc<TmdbClient>,
}

impl TmdbProvider {
    pub fn new(client: Arc<TmdbClient>) -> Self {
        Self { client }
    }
}

#[async_trait]
impl MetadataProvider for TmdbProvider {
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError> {
        let cleaned_title = clean_title_for_search(&query.keyword);
        let params = DiscoverBangumiParams {
            with_text_query: Some(cleaned_title),
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
                    season: None, // TMDB doesn't provide season info in search results
                    platform: None, // TMDB doesn't provide platform info
                    total_episodes: 0, // Not available in search results
                    poster_url: show.poster_path.map(|path| format!("{}{}", TMDB_IMAGE_BASE_URL, path)),
                    air_date: show.first_air_date,
                }
            })
            .collect())
    }

    fn name(&self) -> &'static str {
        "tmdb"
    }
}

/// Clean title for TMDB search by removing season and split-cour markers.
///
/// This is a simplified version that handles common patterns.
fn clean_title_for_search(title: &str) -> String {
    use std::sync::LazyLock;
    use regex::Regex;

    // Pattern to match split-cour markers: 第Xクール, 第X部分, Part X
    static SPLIT_COUR_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"(?i)(?:第\s*\d+\s*(?:クール|部分))|(?:Part\s*\d+)").unwrap()
    });

    // Pattern to match season markers: 第X季, 第X期, SX, Season X
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
