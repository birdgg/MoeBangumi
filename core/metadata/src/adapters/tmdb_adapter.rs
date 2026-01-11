//! TMDB metadata provider adapter

use std::sync::Arc;

use async_trait::async_trait;
use tmdb::{DiscoverBangumiParams, TmdbClient};

use crate::{
    Episode, EpisodeType, MetadataProvider, MetadataSource, Platform, ProviderError, SearchQuery,
    SearchedMetadata,
};

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
                    poster_url: show
                        .poster_path
                        .map(|path| format!("{}{}", TMDB_IMAGE_BASE_URL, path)),
                    air_date: show.first_air_date,
                }
            })
            .collect())
    }

    async fn get_detail(
        &self,
        external_id: &str,
    ) -> Result<Option<SearchedMetadata>, ProviderError> {
        let id: i64 = external_id
            .parse()
            .map_err(|_| ProviderError::InvalidId(external_id.to_string()))?;

        // Try to get TV series details first (more common case for anime)
        if let Ok(tv_detail) = self.client.get_tv_series(id).await {
            let year = tv_detail
                .first_air_date
                .as_ref()
                .and_then(|date| date.split('-').next())
                .and_then(|y| y.parse::<i32>().ok());

            return Ok(Some(SearchedMetadata {
                source: MetadataSource::Tmdb,
                external_id: tv_detail.id.to_string(),
                title_chinese: Some(tv_detail.name.clone()),
                title_original: Some(tv_detail.original_name),
                year,
                season: None,
                platform: Some(Platform::Tv),
                total_episodes: tv_detail.number_of_episodes,
                poster_url: tv_detail
                    .poster_path
                    .map(|path| format!("{}{}", TMDB_IMAGE_BASE_URL, path)),
                air_date: tv_detail.first_air_date,
            }));
        }

        // Fall back to movie details
        if let Ok(movie_detail) = self.client.get_movie(id).await {
            let year = movie_detail
                .release_date
                .as_ref()
                .and_then(|date| date.split('-').next())
                .and_then(|y| y.parse::<i32>().ok());

            return Ok(Some(SearchedMetadata {
                source: MetadataSource::Tmdb,
                external_id: movie_detail.id.to_string(),
                title_chinese: Some(movie_detail.title.clone()),
                title_original: Some(movie_detail.original_title),
                year,
                season: None,
                platform: Some(Platform::Movie),
                total_episodes: 1,
                poster_url: movie_detail
                    .poster_path
                    .map(|path| format!("{}{}", TMDB_IMAGE_BASE_URL, path)),
                air_date: movie_detail.release_date,
            }));
        }

        Ok(None)
    }

    async fn get_episodes(&self, external_id: &str) -> Result<Vec<Episode>, ProviderError> {
        let id: i64 = external_id
            .parse()
            .map_err(|_| ProviderError::InvalidId(external_id.to_string()))?;

        // Get TV series details to find all seasons
        let tv_detail = match self.client.get_tv_series(id).await {
            Ok(detail) => detail,
            Err(_) => return Ok(vec![]), // Not a TV series, return empty
        };

        let mut all_episodes = Vec::new();

        // Iterate through all seasons to get episodes
        for season in &tv_detail.seasons {
            // Skip season 0 (specials)
            if season.season_number == 0 {
                continue;
            }

            let season_detail = match self.client.get_season(id, season.season_number).await {
                Ok(detail) => detail,
                Err(e) => {
                    tracing::warn!(
                        "Failed to get season {} for TMDB series {}: {}",
                        season.season_number,
                        id,
                        e
                    );
                    continue;
                }
            };

            for ep in season_detail.episodes {
                all_episodes.push(Episode {
                    id: ep.id,
                    episode_type: EpisodeType::Main,
                    name: ep.name.clone(),
                    name_cn: ep.name,
                    sort: ep.episode_number as f64,
                    ep: Some(ep.episode_number as f64),
                    air_date: ep.air_date.unwrap_or_default(),
                });
            }
        }

        Ok(all_episodes)
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

    // Pattern to match season markers: 第X季, 第X期, Season X
    // Note: We don't match standalone "S\d" patterns as they may cause false positives
    // (e.g., "Studio" or other words starting with S)
    static SEASON_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"(?i)(?:第[0-9一二三四五六七八九十]+(?:季|期))|(?:SEASON\s*\d{1,2})").unwrap()
    });

    let mut result = title.to_string();
    result = SPLIT_COUR_PATTERN.replace_all(&result, "").to_string();
    result = SEASON_PATTERN.replace_all(&result, "").to_string();
    result.split_whitespace().collect::<Vec<_>>().join(" ")
}
