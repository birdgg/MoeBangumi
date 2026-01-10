//! Unified metadata client

use std::sync::Arc;

use bgmtv::BgmtvClient;
use tmdb::TmdbClient;

use crate::error::MetadataError;
use crate::models::{
    extract_year, tmdb_image_url, EpisodeInfo, MediaType, MetadataSource, MovieDetail,
    SearchResult, SeasonDetail, SeasonSummary, TvDetail,
};

/// Unified metadata client that combines TMDB and BGM.tv
pub struct MetadataClient {
    tmdb: Arc<TmdbClient>,
    bgmtv: Arc<BgmtvClient>,
}

impl MetadataClient {
    /// Create a new MetadataClient
    pub fn new(tmdb: Arc<TmdbClient>, bgmtv: Arc<BgmtvClient>) -> Self {
        Self { tmdb, bgmtv }
    }

    /// Search for TV shows and movies
    ///
    /// Searches TMDB first; if no results, falls back to BGM.tv
    pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>, MetadataError> {
        // Try TMDB first
        let tmdb_results = self.search_tmdb(query).await?;
        if !tmdb_results.is_empty() {
            return Ok(tmdb_results);
        }

        // Fallback to BGM.tv
        self.search_bgmtv(query).await
    }

    /// Search TMDB only
    async fn search_tmdb(&self, query: &str) -> Result<Vec<SearchResult>, MetadataError> {
        let response = self.tmdb.search_multi(query).await?;

        Ok(response
            .results
            .into_iter()
            .map(|item| match item {
                tmdb::models::SearchMultiResult::Tv(tv) => SearchResult {
                    source: MetadataSource::Tmdb,
                    id: tv.id.to_string(),
                    media_type: MediaType::Tv,
                    title: tv.name,
                    original_title: Some(tv.original_name),
                    overview: Some(tv.overview),
                    poster_url: tmdb_image_url(&tv.poster_path),
                    date: tv.first_air_date.clone(),
                    year: extract_year(&tv.first_air_date),
                    vote_average: Some(tv.vote_average),
                },
                tmdb::models::SearchMultiResult::Movie(movie) => SearchResult {
                    source: MetadataSource::Tmdb,
                    id: movie.id.to_string(),
                    media_type: MediaType::Movie,
                    title: movie.title,
                    original_title: Some(movie.original_title),
                    overview: Some(movie.overview),
                    poster_url: tmdb_image_url(&movie.poster_path),
                    date: movie.release_date.clone(),
                    year: extract_year(&movie.release_date),
                    vote_average: Some(movie.vote_average),
                },
            })
            .collect())
    }

    /// Search BGM.tv only
    async fn search_bgmtv(&self, query: &str) -> Result<Vec<SearchResult>, MetadataError> {
        let results = self.bgmtv.search_bangumi(query).await?;

        Ok(results
            .into_iter()
            .map(|subject| {
                let year = subject.date.split('-').next().and_then(|y| y.parse().ok());

                let title = if subject.name_cn.is_empty() {
                    subject.name.clone()
                } else {
                    subject.name_cn.clone()
                };

                SearchResult {
                    source: MetadataSource::Bgmtv,
                    id: subject.id.to_string(),
                    media_type: MediaType::Tv, // BGM.tv subjects are typically anime (TV)
                    title,
                    original_title: Some(subject.name),
                    overview: None, // BGM.tv search doesn't return overview
                    poster_url: Some(subject.images.large),
                    date: Some(subject.date),
                    year,
                    vote_average: None, // BGM.tv search doesn't return score
                }
            })
            .collect())
    }

    /// Get TV show details from TMDB
    pub async fn get_tv_detail(&self, id: i64) -> Result<TvDetail, MetadataError> {
        let tv = self.tmdb.get_tv_details(id).await?;

        Ok(TvDetail {
            source: MetadataSource::Tmdb,
            id: tv.id.to_string(),
            title: tv.name,
            original_title: Some(tv.original_name),
            overview: Some(tv.overview),
            poster_url: tmdb_image_url(&tv.poster_path),
            backdrop_url: tmdb_image_url(&tv.backdrop_path),
            first_air_date: tv.first_air_date.clone(),
            year: extract_year(&tv.first_air_date),
            number_of_seasons: Some(tv.number_of_seasons as i32),
            number_of_episodes: Some(tv.number_of_episodes as i32),
            status: Some(tv.status),
            vote_average: Some(tv.vote_average),
            genres: tv.genres.into_iter().map(|g| g.name).collect(),
            seasons: tv
                .seasons
                .into_iter()
                .map(|s| SeasonSummary {
                    season_number: s.season_number as i32,
                    name: s.name,
                    episode_count: s.episode_count as i32,
                    air_date: s.air_date,
                    poster_url: tmdb_image_url(&s.poster_path),
                })
                .collect(),
        })
    }

    /// Get season details from TMDB
    pub async fn get_season_detail(
        &self,
        tv_id: i64,
        season_number: i64,
    ) -> Result<SeasonDetail, MetadataError> {
        let season = self.tmdb.get_season_details(tv_id, season_number).await?;

        Ok(SeasonDetail {
            source: MetadataSource::Tmdb,
            tv_id: tv_id.to_string(),
            season_number: season.season_number as i32,
            name: season.name,
            overview: Some(season.overview),
            poster_url: tmdb_image_url(&season.poster_path),
            air_date: season.air_date,
            episodes: season
                .episodes
                .into_iter()
                .map(|ep| EpisodeInfo {
                    id: ep.id.to_string(),
                    episode_number: ep.episode_number as i32,
                    name: ep.name,
                    overview: Some(ep.overview),
                    air_date: ep.air_date,
                    still_url: tmdb_image_url(&ep.still_path),
                    runtime: ep.runtime.map(|r| r as i32),
                    vote_average: Some(ep.vote_average),
                })
                .collect(),
        })
    }

    /// Get movie details from TMDB
    pub async fn get_movie_detail(&self, id: i64) -> Result<MovieDetail, MetadataError> {
        let movie = self.tmdb.get_movie_details(id).await?;

        Ok(MovieDetail {
            source: MetadataSource::Tmdb,
            id: movie.id.to_string(),
            title: movie.title,
            original_title: Some(movie.original_title),
            overview: Some(movie.overview),
            poster_url: tmdb_image_url(&movie.poster_path),
            backdrop_url: tmdb_image_url(&movie.backdrop_path),
            release_date: movie.release_date.clone(),
            year: extract_year(&movie.release_date),
            runtime: movie.runtime.map(|r| r as i32),
            status: movie.status,
            vote_average: Some(movie.vote_average),
            genres: movie.genres.into_iter().map(|g| g.name).collect(),
        })
    }

    /// Get access to the underlying TMDB client
    pub fn tmdb(&self) -> &TmdbClient {
        &self.tmdb
    }

    /// Get access to the underlying BGM.tv client
    pub fn bgmtv(&self) -> &BgmtvClient {
        &self.bgmtv
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parking_lot::RwLock;

    fn create_test_client() -> MetadataClient {
        dotenvy::dotenv().ok();
        let api_key = std::env::var("TMDB_API").expect("TMDB_API must be set in .env");

        let http_client = reqwest::Client::new();
        let tmdb_api_key: tmdb::ApiKey = Arc::new(RwLock::new(api_key));
        let tmdb = Arc::new(TmdbClient::new(http_client.clone(), tmdb_api_key));
        let bgmtv = Arc::new(BgmtvClient::new(http_client));

        MetadataClient::new(tmdb, bgmtv)
    }

    #[tokio::test]
    async fn test_search_rezero() {
        let client = create_test_client();
        let results = client.search("Re:从零开始的异世界生活").await.unwrap();

        assert!(!results.is_empty(), "Should find results for Re:Zero");

        // Print results for debugging
        for result in &results {
            println!(
                "[{}] {} - {} ({:?})",
                result.source, result.id, result.title, result.year
            );
        }

        // Check that we got TV results (Re:Zero is a TV series)
        let tv_results: Vec<_> = results
            .iter()
            .filter(|r| r.media_type == MediaType::Tv)
            .collect();
        assert!(!tv_results.is_empty(), "Should have TV results");
    }

