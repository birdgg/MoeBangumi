use serde::{Deserialize, Serialize};
use std::str::FromStr;
use utoipa::ToSchema;

use super::Clearable;

/// Source type for bangumi
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize, ToSchema)]
#[serde(rename_all = "lowercase")]
pub enum SourceType {
    #[default]
    WebRip,
    BDRip,
}

impl SourceType {
    pub fn as_str(&self) -> &'static str {
        match self {
            SourceType::WebRip => "webrip",
            SourceType::BDRip => "bdrip",
        }
    }
}

impl FromStr for SourceType {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "bdrip" => SourceType::BDRip,
            _ => SourceType::WebRip,
        })
    }
}

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

/// Bangumi (anime) main entity
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Bangumi {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Chinese title (primary display)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
    /// Original Chinese title (native language, required, unique)
    pub title_original_chinese: String,
    /// Original Japanese title
    pub title_original_japanese: Option<String>,
    /// Season number
    pub season: i32,
    /// Year
    pub year: i32,

    /// Bangumi.tv ID
    pub bgmtv_id: Option<i64>,
    /// TMDB ID
    pub tmdb_id: Option<i64>,

    /// Poster URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format, required)
    pub air_date: String,
    /// Day of week when new episodes air (0=Sunday, 1=Monday, ..., 6=Saturday, required)
    pub air_week: i32,
    /// Total episodes (0=unknown)
    pub total_episodes: i32,
    /// Episode offset
    pub episode_offset: i32,

    /// Current downloaded episode
    pub current_episode: i32,
    /// Auto download new episodes
    pub auto_download: bool,

    /// Save path (required)
    pub save_path: String,

    /// Source type: webrip or bdrip
    pub source_type: SourceType,

    /// Whether the bangumi has finished airing
    pub finished: bool,

    /// Platform type (TV, Movie, OVA)
    pub platform: Platform,
}

/// RSS entry for creating bangumi with subscriptions
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct RssEntry {
    /// RSS feed URL
    pub url: String,
    /// Regex patterns to exclude from matching
    #[serde(default)]
    pub filters: Vec<String>,
    /// Whether this is the primary RSS source (default: false)
    #[serde(default)]
    pub is_primary: bool,
    /// Optional subtitle group name
    #[serde(default)]
    pub group: Option<String>,
}

/// Request body for creating a new bangumi
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateBangumi {
    /// Chinese title (required)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
    /// Original Chinese title (native language, required, unique)
    pub title_original_chinese: String,
    /// Original Japanese title
    pub title_original_japanese: Option<String>,
    /// Season number (default: 1)
    #[serde(default = "default_season")]
    pub season: i32,
    /// Year (required)
    pub year: i32,
    /// Bangumi.tv ID
    pub bgmtv_id: Option<i64>,
    /// TMDB ID
    pub tmdb_id: Option<i64>,
    /// Poster URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format, required)
    pub air_date: String,
    /// Day of week when new episodes air (0=Sunday, 1=Monday, ..., 6=Saturday, required)
    pub air_week: i32,
    /// Total episodes
    #[serde(default)]
    pub total_episodes: i32,
    /// Episode offset
    #[serde(default)]
    pub episode_offset: i32,
    /// Auto download new episodes
    #[serde(default = "default_auto_download")]
    pub auto_download: bool,
    /// Source type
    #[serde(default)]
    pub source_type: SourceType,
    /// Whether the bangumi has finished airing
    #[serde(default)]
    pub finished: bool,
    /// Platform type (TV, Movie, OVA)
    #[serde(default)]
    pub platform: Platform,
    /// RSS subscriptions to create with this bangumi
    #[serde(default)]
    pub rss_entries: Vec<RssEntry>,
    /// Save path (auto-generated by backend, not exposed in API)
    #[serde(skip_deserializing, default)]
    #[schema(read_only)]
    pub save_path: String,
}

fn default_season() -> i32 {
    1
}

fn default_auto_download() -> bool {
    true
}

/// Bangumi with its RSS subscriptions
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct BangumiWithRss {
    #[serde(flatten)]
    pub bangumi: Bangumi,
    /// RSS subscriptions for this bangumi
    pub rss_entries: Vec<super::Rss>,
}

/// Request body for updating a bangumi with RSS entries
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct UpdateBangumiRequest {
    /// Episode offset
    pub episode_offset: Option<i32>,
    /// Auto download new episodes
    pub auto_download: Option<bool>,
    /// First air date (None = unchanged, Some = new value)
    pub air_date: Option<String>,
    /// Day of week when new episodes air (None = unchanged, Some = new value)
    pub air_week: Option<i32>,
    /// RSS entries to sync (replaces all existing entries)
    pub rss_entries: Option<Vec<RssEntry>>,
}

/// Request body for updating a bangumi.
/// Fields use `Clearable` for optional fields that can be cleared,
/// and `Option` for required fields (None = unchanged, Some = new value).
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateBangumi {
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
    pub bgmtv_id: Clearable<i64>,
    #[serde(default)]
    pub tmdb_id: Clearable<i64>,
    #[serde(default)]
    pub poster_url: Clearable<String>,
    /// First air date (required field, cannot be cleared)
    #[serde(default)]
    pub air_date: Option<String>,
    /// Day of week when new episodes air (required field, cannot be cleared)
    #[serde(default)]
    pub air_week: Option<i32>,
    #[serde(default)]
    pub total_episodes: Option<i32>,
    #[serde(default)]
    pub episode_offset: Option<i32>,
    #[serde(default)]
    pub current_episode: Option<i32>,
    #[serde(default)]
    pub auto_download: Option<bool>,
    #[serde(default)]
    pub source_type: Option<SourceType>,
    #[serde(default)]
    pub finished: Option<bool>,
    #[serde(default)]
    pub platform: Option<Platform>,
}
