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
    /// First air date (YYYY-MM-DD format)
    pub air_date: Option<String>,
    /// Day of week when new episodes air (0=Sunday, 1=Monday, ..., 6=Saturday)
    pub air_week: Option<i32>,
    /// Total episodes (0=unknown)
    pub total_episodes: i32,
    /// Episode offset
    pub episode_offset: i32,

    /// Current downloaded episode
    pub current_episode: i32,
    /// Auto download new episodes
    pub auto_download: bool,

    /// Custom save path (None=use default)
    pub save_path: Option<String>,

    /// Source type: webrip or bdrip
    pub source_type: SourceType,

    /// Whether the bangumi has finished airing
    pub finished: bool,

    /// Kind of bangumi (e.g., TV, Movie, OVA)
    pub kind: Option<String>,
}

/// Request body for creating a new bangumi
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateBangumi {
    /// Chinese title (required)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
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
    /// First air date (YYYY-MM-DD format)
    pub air_date: Option<String>,
    /// Day of week when new episodes air (0=Sunday, 1=Monday, ..., 6=Saturday)
    pub air_week: Option<i32>,
    /// Total episodes
    #[serde(default)]
    pub total_episodes: i32,
    /// Episode offset
    #[serde(default)]
    pub episode_offset: i32,
    /// Auto download new episodes
    #[serde(default = "default_auto_download")]
    pub auto_download: bool,
    /// Custom save path
    pub save_path: Option<String>,
    /// Source type
    #[serde(default)]
    pub source_type: SourceType,
    /// Whether the bangumi has finished airing
    #[serde(default)]
    pub finished: bool,

    /// Kind of bangumi (e.g., TV, Movie, OVA)
    #[serde(default = "default_kind")]
    pub kind: Option<String>,
}

fn default_season() -> i32 {
    1
}

fn default_auto_download() -> bool {
    true
}

fn default_kind() -> Option<String> {
    Some("TV".to_string())
}

/// Request body for updating a bangumi.
/// Fields use `Clearable` to distinguish between:
/// - Not provided (unchanged)
/// - Explicitly set to null (clear)
/// - Set to a new value
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateBangumi {
    #[serde(default)]
    pub title_chinese: Option<String>,
    #[serde(default)]
    pub title_japanese: Clearable<String>,
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
    #[serde(default)]
    pub air_date: Clearable<String>,
    #[serde(default)]
    pub air_week: Clearable<i32>,
    #[serde(default)]
    pub total_episodes: Option<i32>,
    #[serde(default)]
    pub episode_offset: Option<i32>,
    #[serde(default)]
    pub current_episode: Option<i32>,
    #[serde(default)]
    pub auto_download: Option<bool>,
    #[serde(default)]
    pub save_path: Clearable<String>,
    #[serde(default)]
    pub source_type: Option<SourceType>,
    #[serde(default)]
    pub finished: Option<bool>,
    #[serde(default)]
    pub kind: Clearable<String>,
}
