use serde::{Deserialize, Serialize};
use std::str::FromStr;
use super::Clearable;

// Re-export Platform from metadata crate to avoid duplication
pub use metadata::Platform;

/// Source type for bangumi
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
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

/// Bangumi (anime) subscription entity
/// Stores subscription state and metadata (merged from former metadata table)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bangumi {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    // External service IDs
    /// Mikan bangumi ID
    pub mikan_id: Option<String>,
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,
    /// TMDB ID
    pub tmdb_id: Option<i64>,

    // Titles
    /// Chinese title (primary display)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,

    // Basic info
    /// Season number
    pub season: i32,
    /// Year
    pub year: i32,
    /// Platform type (TV, Movie, OVA)
    pub platform: Platform,

    // Metadata
    /// Total episodes (0=unknown)
    pub total_episodes: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format)
    pub air_date: Option<String>,
    /// Day of week when new episodes air (0=Sunday ~ 6=Saturday)
    pub air_week: i32,
    /// Last TMDB lookup attempt timestamp
    pub tmdb_lookup_at: Option<chrono::DateTime<chrono::Utc>>,
    /// Episode offset for season-relative numbering
    pub episode_offset: i32,

    // Subscription state
    /// Current downloaded episode
    pub current_episode: i32,
    /// Only download first matching episode per RSS check
    pub auto_complete: bool,
    /// Save path (required)
    pub save_path: String,
    /// Source type: webrip or bdrip
    pub source_type: SourceType,
}

impl Bangumi {
    /// Adjust episode number by applying episode_offset.
    ///
    /// Converts RSS episode number to season-relative episode number.
    /// Only applies offset if episode > offset, otherwise returns original episode.
    ///
    /// # Example
    /// ```ignore
    /// // RSS shows episode 13, offset is 12 -> returns 1 (s02e01)
    /// let adjusted = bangumi.adjust_episode(13); // returns 1
    ///
    /// // RSS shows episode 5, offset is 12 -> returns 5 (offset not applied)
    /// let adjusted = bangumi.adjust_episode(5); // returns 5
    /// ```
    pub fn adjust_episode(&self, episode: i32) -> i32 {
        if episode > self.episode_offset {
            episode - self.episode_offset
        } else {
            episode
        }
    }
}

/// RSS entry for creating bangumi with subscriptions
#[derive(Debug, Clone, Serialize, Deserialize)]
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
pub struct CreateBangumi {
    // External IDs
    /// Mikan bangumi ID
    pub mikan_id: Option<String>,
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,
    /// TMDB ID
    pub tmdb_id: Option<i64>,

    // Titles
    /// Chinese title (required)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,

    // Basic info
    /// Season number (default: 1)
    #[serde(default = "default_season")]
    pub season: i32,
    /// Year (required)
    pub year: i32,
    /// Platform type (TV, Movie, OVA)
    #[serde(default)]
    pub platform: Platform,

    // Metadata
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
    /// Episode offset for season-relative numbering
    #[serde(default)]
    pub episode_offset: i32,

    // Subscription config
    /// Only download first matching episode per RSS check
    #[serde(default = "default_auto_complete")]
    pub auto_complete: bool,
    /// Source type
    #[serde(default)]
    pub source_type: SourceType,
    /// Initial current episode (for imported bangumi with existing episodes)
    #[serde(default)]
    pub current_episode: Option<i32>,

    /// RSS subscriptions to create with this bangumi
    #[serde(default)]
    pub rss_entries: Vec<RssEntry>,

    /// Save path (auto-generated by backend, not exposed in API)
    #[serde(skip_deserializing, default)]
        pub save_path: String,
}

fn default_season() -> i32 {
    1
}

fn default_auto_complete() -> bool {
    true
}

impl CreateBangumi {
    /// Convert CreateBangumi to UpdateBangumi for merging with existing bangumi.
    ///
    /// # Conversion Rules
    /// - Required fields: Always set as `Some(value)`
    /// - Optional fields with `Some(value)`: Converted to `Clearable::Set(value)`
    /// - Optional fields with `None`: Converted to `Clearable::Unchanged` (preserves existing value)
    pub fn into_update(self) -> UpdateBangumi {
        UpdateBangumi {
            // Subscription fields
            current_episode: self.current_episode,
            auto_complete: Some(self.auto_complete),
            source_type: Some(self.source_type),
            // External IDs
            mikan_id: match self.mikan_id {
                Some(id) => Clearable::Set(id),
                None => Clearable::Unchanged,
            },
            bgmtv_id: match self.bgmtv_id {
                Some(id) => Clearable::Set(id),
                None => Clearable::Unchanged,
            },
            tmdb_id: match self.tmdb_id {
                Some(id) => Clearable::Set(id),
                None => Clearable::Unchanged,
            },
            // Titles
            title_chinese: Some(self.title_chinese),
            title_japanese: match self.title_japanese {
                Some(title) => Clearable::Set(title),
                None => Clearable::Unchanged,
            },
            // Basic info
            season: Some(self.season),
            year: Some(self.year),
            platform: Some(self.platform),
            // Metadata
            total_episodes: Some(self.total_episodes),
            poster_url: match self.poster_url {
                Some(url) => Clearable::Set(url),
                None => Clearable::Unchanged,
            },
            air_date: match self.air_date {
                Some(date) => Clearable::Set(date),
                None => Clearable::Unchanged,
            },
            air_week: Some(self.air_week),
            episode_offset: Some(self.episode_offset),
        }
    }
}

/// Bangumi with RSS subscriptions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BangumiWithRss {
    #[serde(flatten)]
    pub bangumi: Bangumi,
    /// RSS subscriptions for this bangumi
    pub rss_entries: Vec<super::Rss>,
}

/// Request body for updating a bangumi with RSS entries
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateBangumiRequest {
    /// Only download first matching episode per RSS check
    pub auto_complete: Option<bool>,
    /// Episode offset for season-relative numbering
    pub episode_offset: Option<i32>,
    /// RSS entries to sync (replaces all existing entries)
    pub rss_entries: Option<Vec<RssEntry>>,
}

/// Request body for updating a bangumi
/// Includes both subscription-related fields and metadata fields
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateBangumi {
    // Subscription fields
    #[serde(default)]
    pub current_episode: Option<i32>,
    #[serde(default)]
    pub auto_complete: Option<bool>,
    #[serde(default)]
    pub source_type: Option<SourceType>,

    // External IDs
    /// Mikan bangumi ID (null to clear)
    #[serde(default)]
        pub mikan_id: Clearable<String>,
    /// BGM.tv subject ID (null to clear)
    #[serde(default)]
        pub bgmtv_id: Clearable<i64>,
    /// TMDB ID (null to clear)
    #[serde(default)]
        pub tmdb_id: Clearable<i64>,

    // Titles
    /// Chinese title
    #[serde(default)]
    pub title_chinese: Option<String>,
    /// Japanese original name (null to clear)
    #[serde(default)]
        pub title_japanese: Clearable<String>,

    // Basic info
    /// Season number
    #[serde(default)]
    pub season: Option<i32>,
    /// Year
    #[serde(default)]
    pub year: Option<i32>,
    /// Platform type (TV, Movie, OVA)
    #[serde(default)]
    pub platform: Option<Platform>,

    // Metadata
    /// Total episodes
    #[serde(default)]
    pub total_episodes: Option<i32>,
    /// Poster image URL (null to clear)
    #[serde(default)]
        pub poster_url: Clearable<String>,
    /// First air date in YYYY-MM-DD format (null to clear)
    #[serde(default)]
        pub air_date: Clearable<String>,
    /// Day of week when new episodes air (0=Sunday ~ 6=Saturday)
    #[serde(default)]
    pub air_week: Option<i32>,
    /// Episode offset for season-relative numbering
    #[serde(default)]
    pub episode_offset: Option<i32>,
}
