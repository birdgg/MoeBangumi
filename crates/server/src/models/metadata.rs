use serde::{Deserialize, Serialize};
use std::str::FromStr;
use utoipa::ToSchema;

use super::Clearable;

/// Platform type for bangumi (TV, Movie, OVA)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize, ToSchema)]
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
            "movie" => Platform::Movie,
            "ova" => Platform::Ova,
            _ => Platform::Tv,
        })
    }
}

/// Metadata entity for anime information
/// Unified metadata center caching data from BGM.tv, TMDB, and Mikan
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Metadata {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Mikan bangumi ID
    pub mikan_id: Option<String>,
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,
    /// TMDB ID
    pub tmdb_id: Option<i64>,

    /// Chinese title (primary display)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
    /// Original Chinese title (from API)
    pub title_original_chinese: String,
    /// Original Japanese title (from API)
    pub title_original_japanese: Option<String>,

    /// Season number
    pub season: i32,
    /// Year
    pub year: i32,
    /// Platform type (TV, Movie, OVA)
    pub platform: Platform,

    /// Total episodes (0=unknown)
    pub total_episodes: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format)
    pub air_date: Option<String>,
    /// Day of week when new episodes air (0=Sunday ~ 6=Saturday)
    pub air_week: i32,
    /// Whether the anime has finished airing
    pub finished: bool,
}

/// Request body for creating new metadata
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateMetadata {
    /// Mikan bangumi ID
    pub mikan_id: Option<String>,
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,
    /// TMDB ID
    pub tmdb_id: Option<i64>,

    /// Chinese title (required)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
    /// Original Chinese title (from API, defaults to title_chinese if not provided)
    pub title_original_chinese: Option<String>,
    /// Original Japanese title (from API)
    pub title_original_japanese: Option<String>,

    /// Season number (default: 1)
    #[serde(default = "default_season")]
    pub season: i32,
    /// Year (required)
    pub year: i32,
    /// Platform type (TV, Movie, OVA)
    #[serde(default)]
    pub platform: Platform,

    /// Total episodes
    #[serde(default)]
    pub total_episodes: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format)
    pub air_date: Option<String>,
    /// Day of week when new episodes air (0=Sunday ~ 6=Saturday)
    pub air_week: i32,
    /// Whether the anime has finished airing
    #[serde(default)]
    pub finished: bool,
}

fn default_season() -> i32 {
    1
}

/// Request body for updating metadata
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateMetadata {
    #[serde(default)]
    pub mikan_id: Clearable<String>,
    #[serde(default)]
    pub bgmtv_id: Clearable<i64>,
    #[serde(default)]
    pub tmdb_id: Clearable<i64>,

    #[serde(default)]
    pub title_chinese: Option<String>,
    #[serde(default)]
    pub title_japanese: Clearable<String>,
    #[serde(default)]
    pub title_original_chinese: Option<String>,
    #[serde(default)]
    pub title_original_japanese: Clearable<String>,

    #[serde(default)]
    pub season: Option<i32>,
    #[serde(default)]
    pub year: Option<i32>,
    #[serde(default)]
    pub platform: Option<Platform>,

    #[serde(default)]
    pub total_episodes: Option<i32>,
    #[serde(default)]
    pub poster_url: Clearable<String>,
    #[serde(default)]
    pub air_date: Clearable<String>,
    #[serde(default)]
    pub air_week: Option<i32>,
    #[serde(default)]
    pub finished: Option<bool>,
}
