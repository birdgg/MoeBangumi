//! Data models for metadata search results

use serde::{Deserialize, Serialize};
use std::str::FromStr;
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

/// Platform type for bangumi (TV, Movie, OVA)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    #[default]
    Tv,
    Movie,
    Ova,
}

impl Platform {
    pub fn as_str(&self) -> &'static str {
        match self {
            Platform::Tv => "tv",
            Platform::Movie => "movie",
            Platform::Ova => "ova",
        }
    }

    /// Check if this platform is a movie
    pub fn is_movie(&self) -> bool {
        matches!(self, Platform::Movie)
    }
}

impl FromStr for Platform {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "movie" | "劇場版" => Platform::Movie,
            "ova" => Platform::Ova,
            _ => Platform::Tv,
        })
    }
}

/// Search query parameters
#[derive(Debug, Clone)]
pub struct SearchQuery {
    /// Search keyword (required)
    pub keyword: String,
    /// Year filter (optional)
    pub year: Option<i32>,
}

impl SearchQuery {
    pub fn new(keyword: impl Into<String>) -> Self {
        Self {
            keyword: keyword.into(),
            year: None,
        }
    }

    pub fn with_year(mut self, year: i32) -> Self {
        self.year = Some(year);
        self
    }
}

/// Standardized metadata search result
///
/// This represents metadata fetched from external sources (BGM.tv, TMDB, Mikan)
/// before it is persisted to the database.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SearchedMetadata {
    /// Data source identifier
    pub source: MetadataSource,

    /// External ID (string form to unify i64 and String IDs)
    pub external_id: String,

    /// Chinese title
    pub title_chinese: Option<String>,
    /// Japanese title
    pub title_japanese: Option<String>,

    /// Year
    pub year: Option<i32>,
    /// Season number
    pub season: Option<i32>,
    /// Platform type (TV, Movie, OVA)
    pub platform: Option<Platform>,

    /// Total episodes (0 = unknown)
    pub total_episodes: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format)
    pub air_date: Option<String>,
}

impl SearchedMetadata {
    /// Check if this result matches the given year (with ±1 year tolerance)
    pub fn matches_year(&self, expected_year: i32) -> bool {
        self.year
            .map(|y| (y - expected_year).abs() <= 1)
            .unwrap_or(false)
    }
}

/// Episode type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum EpisodeType {
    /// Main episode (本篇)
    #[default]
    Main,
    /// Special episode (SP)
    Special,
    /// Opening
    Opening,
    /// Ending
    Ending,
}

/// Combined search results from all metadata sources
///
/// This struct holds results from parallel searches to BGM.tv and TMDB,
/// allowing the frontend to display results grouped by data source.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CombinedSearchResults {
    /// Search results from BGM.tv
    pub bgmtv: Vec<SearchedMetadata>,
    /// Search results from TMDB
    pub tmdb: Vec<SearchedMetadata>,
}

/// Episode information from metadata provider
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Episode {
    /// Episode ID from the source
    pub id: i64,
    /// Episode type
    pub episode_type: EpisodeType,
    /// Original name
    pub name: String,
    /// Chinese name
    pub name_cn: String,
    /// Sort order (absolute episode number)
    pub sort: f64,
    /// Episode number (season-relative)
    pub ep: Option<f64>,
    /// Air date (YYYY-MM-DD format)
    pub air_date: String,
}
