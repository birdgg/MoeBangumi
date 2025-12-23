use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

/// RSS subscription entity
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Rss {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Foreign key to bangumi
    pub bangumi_id: i64,
    /// RSS subscription title
    pub title: String,
    /// RSS feed URL
    pub url: String,
    /// Download save path
    pub save_path: String,
    /// Whether subscription is enabled
    pub enabled: bool,
    /// Regex patterns to exclude from matching
    pub exclude_filters: Vec<String>,
}

/// Request body for creating a new RSS subscription
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateRss {
    /// Foreign key to bangumi
    pub bangumi_id: i64,
    /// RSS subscription title
    pub title: String,
    /// RSS feed URL
    pub url: String,
    /// Download save path
    pub save_path: String,
    /// Whether subscription is enabled (default: true)
    #[serde(default = "default_enabled")]
    pub enabled: bool,
    /// Regex patterns to exclude from matching
    #[serde(default)]
    pub exclude_filters: Vec<String>,
}

fn default_enabled() -> bool {
    true
}

/// Request body for updating an RSS subscription
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateRss {
    #[serde(default)]
    pub title: Option<String>,
    #[serde(default)]
    pub url: Option<String>,
    #[serde(default)]
    pub save_path: Option<String>,
    #[serde(default)]
    pub enabled: Option<bool>,
    #[serde(default)]
    pub exclude_filters: Option<Vec<String>>,
}
