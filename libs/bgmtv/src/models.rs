use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

/// BGM.tv subject type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize_repr, Deserialize_repr)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[repr(i32)]
pub enum SubjectType {
    #[default]
    Book = 1,
    Anime = 2,
    Music = 3,
    Game = 4,
    Real = 6,
}

/// Search request body for POST /v0/search/subjects
#[derive(Debug, Clone, Serialize)]
pub struct SearchSubjectsRequest {
    pub keyword: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filter: Option<SearchFilter>,
}

/// Search filter options
#[derive(Debug, Clone, Default, Serialize)]
pub struct SearchFilter {
    /// Subject type filter
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub subject_type: Option<Vec<SubjectType>>,
    /// Meta tags filter (e.g., "日本")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub meta_tags: Option<Vec<String>>,
    /// Air date filters (e.g., [">=2020-07-01", "<2020-10-01"])
    #[serde(skip_serializing_if = "Option::is_none")]
    pub air_date: Option<Vec<String>>,
}

/// Search response from POST /v0/search/subjects
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SearchSubjectsResponse {
    pub total: i64,
    pub limit: i64,
    pub offset: i64,
    pub data: Vec<Subject>,
}

/// Subject images from BGM.tv API
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SubjectImages {
    pub small: String,
    pub grid: String,
    pub large: String,
    pub medium: String,
    pub common: String,
}

/// Subject item in search results
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Subject {
    pub id: i64,
    pub name: String,
    pub name_cn: String,
    pub date: String,
    pub platform: String,
    pub images: SubjectImages,
    pub image: String,
    pub eps: i64,
}

/// Episode type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize_repr, Deserialize_repr)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[repr(i32)]
pub enum EpisodeType {
    /// 本篇
    #[default]
    Main = 0,
    /// SP
    Special = 1,
    /// OP
    Opening = 2,
    /// ED
    Ending = 3,
}

/// Episode item
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Episode {
    pub id: i64,
    /// Episode type: 0=本篇, 1=SP, 2=OP, 3=ED
    #[serde(rename = "type")]
    pub episode_type: EpisodeType,
    /// Original name
    pub name: String,
    /// Chinese name
    pub name_cn: String,
    /// Sort order
    pub sort: f64,
    /// Episode number
    pub ep: Option<f64>,
    /// Air date
    pub airdate: String,
}

/// Episodes API response from GET /v0/episodes
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct EpisodesResponse {
    pub data: Vec<Episode>,
    pub total: i64,
    pub limit: i64,
    pub offset: i64,
}

/// Weekday information for calendar
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Weekday {
    pub en: String,
    pub cn: String,
    pub ja: String,
    pub id: i32,
}

/// Rating information for calendar subject
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CalendarRating {
    pub total: i64,
    pub score: f64,
}

/// Collection information for calendar subject
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CalendarCollection {
    #[serde(default)]
    pub doing: i64,
}

/// Subject item in calendar results
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CalendarSubject {
    pub id: i64,
    #[serde(rename = "type")]
    pub subject_type: SubjectType,
    pub name: String,
    pub name_cn: String,
    #[serde(default)]
    pub summary: String,
    pub air_date: String,
    pub air_weekday: i32,
    #[serde(default)]
    pub rating: Option<CalendarRating>,
    #[serde(default)]
    pub rank: Option<i64>,
    pub images: SubjectImages,
    #[serde(default)]
    pub collection: Option<CalendarCollection>,
    /// Platform type (TV, Movie, OVA) - not from BGM.tv API, added by our service
    #[serde(default)]
    pub platform: Option<String>,
}

/// Calendar day with weekday info and items
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CalendarDay {
    pub weekday: Weekday,
    pub items: Vec<CalendarSubject>,
}

/// Detailed subject information from GET /v0/subjects/{id}
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SubjectDetail {
    pub id: i64,
    #[serde(rename = "type")]
    pub subject_type: SubjectType,
    pub name: String,
    pub name_cn: String,
    pub summary: String,
    pub nsfw: bool,
    pub locked: bool,
    #[serde(default)]
    pub date: Option<String>,
    #[serde(default)]
    pub platform: Option<String>,
    pub images: SubjectImages,
    pub volumes: i64,
    pub eps: i64,
    pub total_episodes: i64,
    #[serde(default)]
    pub meta_tags: Vec<String>,
    pub series: bool,
}
