use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};

/// BGM.tv subject type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize_repr, Deserialize_repr)]
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
}

/// Search response from POST /v0/search/subjects
#[derive(Debug, Clone, Deserialize)]
pub struct SearchSubjectsResponse {
    pub total: i64,
    pub limit: i64,
    pub offset: i64,
    pub data: Vec<Subject>,
}

/// Subject item in search results
#[derive(Debug, Clone, Deserialize)]
pub struct Subject {
    pub id: i64,
    pub name: String,
    pub name_cn: String,
    pub date: Option<String>,
    pub platform: Option<String>,
    pub image: Option<String>,
    pub eps: i64,
}
