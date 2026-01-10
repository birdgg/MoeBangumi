use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

use super::Clearable;

/// Series entity representing an anime franchise
/// (e.g., "Re:Zero - Starting Life in Another World")
/// Used to group different seasons of the same anime
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Series {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// TMDB TV Show ID
    pub tmdb_id: Option<i64>,

    /// Chinese title (primary display)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
    /// Poster image URL
    pub poster_url: Option<String>,
}

/// Request body for creating a new series
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CreateSeries {
    /// TMDB TV Show ID
    pub tmdb_id: Option<i64>,
    /// Chinese title (required)
    pub title_chinese: String,
    /// Japanese original name
    pub title_japanese: Option<String>,
    /// Poster image URL
    pub poster_url: Option<String>,
}

/// Request body for updating a series
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateSeries {
    /// TMDB TV Show ID (null to clear)
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
    /// Poster image URL (null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub poster_url: Clearable<String>,
}
