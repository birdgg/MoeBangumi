//! Unified data models for metadata

use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

/// Metadata data source identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum MetadataSource {
    Bgmtv,
    Tmdb,
}

impl std::fmt::Display for MetadataSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetadataSource::Bgmtv => write!(f, "BGM.tv"),
            MetadataSource::Tmdb => write!(f, "TMDB"),
        }
    }
}

/// Media type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum MediaType {
    #[default]
    Tv,
    Movie,
}

/// Search result from metadata providers
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SearchResult {
    /// Data source
    pub source: MetadataSource,
    /// External ID
    pub id: String,
    /// Media type (tv/movie)
    pub media_type: MediaType,
    /// Title (localized)
    pub title: String,
    /// Original title
    pub original_title: Option<String>,
    /// Overview/description
    pub overview: Option<String>,
    /// Poster URL
    pub poster_url: Option<String>,
    /// First air date or release date (YYYY-MM-DD)
    pub date: Option<String>,
    /// Year extracted from date
    pub year: Option<i32>,
    /// Vote average (0-10)
    pub vote_average: Option<f64>,
}

/// TV show detail
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct TvDetail {
    /// Data source
    pub source: MetadataSource,
    /// External ID
    pub id: String,
    /// Title (localized)
    pub title: String,
    /// Original title
    pub original_title: Option<String>,
    /// Overview/description
    pub overview: Option<String>,
    /// Poster URL
    pub poster_url: Option<String>,
    /// Backdrop URL
    pub backdrop_url: Option<String>,
    /// First air date (YYYY-MM-DD)
    pub first_air_date: Option<String>,
    /// Year
    pub year: Option<i32>,
    /// Number of seasons
    pub number_of_seasons: Option<i32>,
    /// Number of episodes
    pub number_of_episodes: Option<i32>,
    /// Status (e.g., "Returning Series", "Ended")
    pub status: Option<String>,
    /// Vote average (0-10)
    pub vote_average: Option<f64>,
    /// Genres
    pub genres: Vec<String>,
    /// Season summaries
    pub seasons: Vec<SeasonSummary>,
}

/// Season summary (from TV detail)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SeasonSummary {
    /// Season number
    pub season_number: i32,
    /// Name
    pub name: String,
    /// Episode count
    pub episode_count: i32,
    /// Air date
    pub air_date: Option<String>,
    /// Poster URL
    pub poster_url: Option<String>,
}

/// Season detail with episodes
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SeasonDetail {
    /// Data source
    pub source: MetadataSource,
    /// TV show ID
    pub tv_id: String,
    /// Season number
    pub season_number: i32,
    /// Name
    pub name: String,
    /// Overview
    pub overview: Option<String>,
    /// Poster URL
    pub poster_url: Option<String>,
    /// Air date
    pub air_date: Option<String>,
    /// Episodes
    pub episodes: Vec<EpisodeInfo>,
}

/// Episode information
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct EpisodeInfo {
    /// Episode ID
    pub id: String,
    /// Episode number
    pub episode_number: i32,
    /// Name
    pub name: String,
    /// Overview
    pub overview: Option<String>,
    /// Air date
    pub air_date: Option<String>,
    /// Still image URL
    pub still_url: Option<String>,
    /// Runtime in minutes
    pub runtime: Option<i32>,
    /// Vote average
    pub vote_average: Option<f64>,
}

/// Movie detail
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct MovieDetail {
    /// Data source
    pub source: MetadataSource,
    /// External ID
    pub id: String,
    /// Title (localized)
    pub title: String,
    /// Original title
    pub original_title: Option<String>,
    /// Overview/description
    pub overview: Option<String>,
    /// Poster URL
    pub poster_url: Option<String>,
    /// Backdrop URL
    pub backdrop_url: Option<String>,
    /// Release date (YYYY-MM-DD)
    pub release_date: Option<String>,
    /// Year
    pub year: Option<i32>,
    /// Runtime in minutes
    pub runtime: Option<i32>,
    /// Status
    pub status: Option<String>,
    /// Vote average (0-10)
    pub vote_average: Option<f64>,
    /// Genres
    pub genres: Vec<String>,
}

// ============ Helper functions ============

/// Extract year from date string (YYYY-MM-DD)
pub fn extract_year(date: &Option<String>) -> Option<i32> {
    date.as_ref()
        .and_then(|d| d.split('-').next())
        .and_then(|y| y.parse().ok())
}

/// TMDB image base URL
pub const TMDB_IMAGE_BASE_URL: &str = "https://image.tmdb.org/t/p/w500";

/// Build TMDB image URL
pub fn tmdb_image_url(path: &Option<String>) -> Option<String> {
    path.as_ref().map(|p| format!("{}{}", TMDB_IMAGE_BASE_URL, p))
}
