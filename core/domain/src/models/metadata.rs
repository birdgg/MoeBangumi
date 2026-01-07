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

/// Metadata entity for anime information
/// Unified metadata center caching data from BGM.tv, TMDB, and Mikan
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
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

    /// Last TMDB lookup attempt timestamp
    pub tmdb_lookup_at: Option<chrono::DateTime<chrono::Utc>>,

    /// Episode offset for season-relative numbering
    pub episode_offset: i32,
}

impl Metadata {
    /// Adjust episode number by applying episode_offset.
    ///
    /// Converts RSS episode number to season-relative episode number.
    /// Only applies offset if episode > offset, otherwise returns original episode.
    ///
    /// # Example
    /// ```ignore
    /// // RSS shows episode 13, offset is 12 -> returns 1 (s02e01)
    /// let adjusted = metadata.adjust_episode(13); // returns 1
    ///
    /// // RSS shows episode 5, offset is 12 -> returns 5 (offset not applied)
    /// let adjusted = metadata.adjust_episode(5); // returns 5
    /// ```
    pub fn adjust_episode(&self, episode: i32) -> i32 {
        if episode > self.episode_offset {
            episode - self.episode_offset
        } else {
            episode
        }
    }
}

/// Request body for creating new metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
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
    /// Episode offset for season-relative numbering
    #[serde(default)]
    pub episode_offset: i32,
}

fn default_season() -> i32 {
    1
}

impl CreateMetadata {
    /// Convert CreateMetadata to UpdateMetadata for merging with existing metadata.
    ///
    /// # Conversion Rules
    /// - Required fields (title_chinese, season, year, platform, etc.): Always set as `Some(value)`
    /// - Optional fields with `Some(value)`: Converted to `Clearable::Set(value)`
    /// - Optional fields with `None`: Converted to `Clearable::Unchanged` (preserves existing value)
    ///
    /// # Use Case
    /// This method is used by `MetadataService::find_or_update()` to update existing metadata
    /// while preserving fields that are not provided in the new data. This is useful when
    /// creating a Bangumi with partial metadata - existing values won't be overwritten.
    ///
    /// # Example
    /// If existing metadata has `tmdb_id = Some(12345)` and new CreateMetadata has
    /// `tmdb_id = None`, the resulting UpdateMetadata will keep the existing TMDB ID.
    pub fn into_update(self) -> UpdateMetadata {
        UpdateMetadata {
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
            title_chinese: Some(self.title_chinese),
            title_japanese: match self.title_japanese {
                Some(title) => Clearable::Set(title),
                None => Clearable::Unchanged,
            },
            season: Some(self.season),
            year: Some(self.year),
            platform: Some(self.platform),
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

/// Request body for updating metadata
/// For Clearable fields: null means clear the value, value means set new value, absent means unchanged
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateMetadata {
    /// Mikan bangumi ID (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub mikan_id: Clearable<String>,
    /// BGM.tv subject ID (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<i64>))]
    pub bgmtv_id: Clearable<i64>,
    /// TMDB ID (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<i64>))]
    pub tmdb_id: Clearable<i64>,

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
    /// Platform type (TV, Movie, OVA)
    #[serde(default)]
    pub platform: Option<Platform>,

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
    /// Episode offset for season-relative numbering
    #[serde(default)]
    pub episode_offset: Option<i32>,
}
