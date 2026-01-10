use serde::{Deserialize, Serialize};
use std::str::FromStr;
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

use super::Clearable;

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
            "movie" => Platform::Movie,
            "ova" => Platform::Ova,
            _ => Platform::Tv,
        })
    }
}

/// Source type for bangumi
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
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

/// Bangumi entity representing a specific season/work
/// (e.g., "Re:Zero Season 2")
/// Merged from old metadata + bangumi tables
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Bangumi {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Reference to series (foreign key)
    pub series_id: i64,

    // -- External IDs --
    /// Mikan bangumi ID
    pub mikan_id: Option<String>,
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,

    // -- Metadata --
    /// Chinese title (primary display)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
    /// Season number
    pub season: i32,
    /// Year
    pub year: i32,
    /// Total episodes (0=unknown)
    pub total_episodes: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format)
    pub air_date: Option<String>,
    /// Day of week when new episodes air (0=Sunday ~ 6=Saturday)
    pub air_week: i32,
    /// Platform type (TV, Movie, OVA)
    pub platform: Platform,

    // -- Subscription config --
    /// Current downloaded episode
    pub current_episode: i32,
    /// Episode offset for season-relative numbering
    pub episode_offset: i32,
    /// Only download first matching episode per RSS check
    pub auto_complete: bool,
    /// Source type: webrip or bdrip
    pub source_type: SourceType,
}

impl Bangumi {
    /// Adjust episode number by applying episode_offset.
    ///
    /// Converts RSS episode number to season-relative episode number.
    /// Only applies offset if episode > offset, otherwise returns original episode.
    pub fn adjust_episode(&self, episode: i32) -> i32 {
        if episode > self.episode_offset {
            episode - self.episode_offset
        } else {
            episode
        }
    }
}

/// Bangumi with its associated series
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct BangumiWithSeries {
    #[serde(flatten)]
    pub bangumi: Bangumi,
    /// Associated series
    pub series: super::Series,
}

/// RSS entry for creating bangumi with subscriptions
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct RssEntry {
    /// RSS feed URL
    pub url: String,
    /// Regex patterns to exclude from matching
    #[serde(default)]
    pub filters: Vec<String>,
    /// Regex patterns to include in matching
    #[serde(default)]
    pub include_filters: Vec<String>,
    /// Optional subtitle group name
    #[serde(default)]
    pub subtitle_group: Option<String>,
}

/// Request body for creating a new bangumi
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CreateBangumi {
    /// Series ID (if using existing series)
    pub series_id: Option<i64>,
    /// Inline series creation (if not using existing)
    pub series: Option<super::CreateSeries>,

    // -- External IDs --
    /// Mikan bangumi ID
    pub mikan_id: Option<String>,
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,

    // -- Metadata --
    /// Chinese title (required)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
    /// Season number (default: 1)
    #[serde(default = "default_season")]
    pub season: i32,
    /// Year (required)
    pub year: i32,
    /// Total episodes
    #[serde(default)]
    pub total_episodes: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format)
    pub air_date: Option<String>,
    /// Day of week when new episodes air (0=Sunday ~ 6=Saturday)
    #[serde(default)]
    pub air_week: i32,
    /// Platform type (TV, Movie, OVA)
    #[serde(default)]
    pub platform: Platform,

    // -- Subscription config --
    /// Episode offset for season-relative numbering
    #[serde(default)]
    pub episode_offset: i32,
    /// Only download first matching episode per RSS check
    #[serde(default = "default_auto_complete")]
    pub auto_complete: bool,
    /// Source type
    #[serde(default)]
    pub source_type: SourceType,
    /// Initial current episode (for imported bangumi with existing episodes)
    #[serde(default)]
    pub current_episode: i32,

    /// RSS subscriptions to create with this bangumi
    #[serde(default)]
    pub rss_entries: Vec<RssEntry>,
}

fn default_season() -> i32 {
    1
}

fn default_auto_complete() -> bool {
    true
}

/// Bangumi with series and RSS subscriptions
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct BangumiWithRss {
    #[serde(flatten)]
    pub bangumi: Bangumi,
    /// Associated series
    pub series: super::Series,
    /// RSS subscriptions for this bangumi
    pub rss_entries: Vec<super::Rss>,
}

/// Request body for updating a bangumi with RSS entries
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateBangumiRequest {
    /// Only download first matching episode per RSS check
    pub auto_complete: Option<bool>,
    /// Episode offset for season-relative numbering
    pub episode_offset: Option<i32>,
    /// RSS entries to sync (replaces all existing entries)
    pub rss_entries: Option<Vec<RssEntry>>,
}

/// Request body for updating a bangumi
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateBangumi {
    // -- External IDs --
    /// Mikan bangumi ID (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub mikan_id: Clearable<String>,
    /// BGM.tv subject ID (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<i64>))]
    pub bgmtv_id: Clearable<i64>,

    // -- Metadata --
    /// Chinese title
    #[serde(default)]
    pub title_chinese: Option<String>,
    /// Japanese original name (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub title_japanese: Clearable<String>,
    /// Season number
    #[serde(default)]
    pub season: Option<i32>,
    /// Year
    #[serde(default)]
    pub year: Option<i32>,
    /// Total episodes
    #[serde(default)]
    pub total_episodes: Option<i32>,
    /// Poster image URL (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub poster_url: Clearable<String>,
    /// First air date in YYYY-MM-DD format (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub air_date: Clearable<String>,
    /// Day of week when new episodes air (0=Sunday ~ 6=Saturday)
    #[serde(default)]
    pub air_week: Option<i32>,
    /// Platform type (TV, Movie, OVA)
    #[serde(default)]
    pub platform: Option<Platform>,

    // -- Subscription config --
    /// Current downloaded episode
    #[serde(default)]
    pub current_episode: Option<i32>,
    /// Episode offset for season-relative numbering
    #[serde(default)]
    pub episode_offset: Option<i32>,
    /// Only download first matching episode per RSS check
    #[serde(default)]
    pub auto_complete: Option<bool>,
    /// Source type
    #[serde(default)]
    pub source_type: Option<SourceType>,
}
