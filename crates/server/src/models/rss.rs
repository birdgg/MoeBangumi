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
    /// RSS feed URL
    pub url: String,
    /// Whether subscription is enabled
    pub enabled: bool,
    /// Regex patterns to exclude from matching
    pub exclude_filters: Vec<String>,
    /// Regex patterns to include (AND logic - title must match ALL patterns if not empty)
    pub include_filters: Vec<String>,
    /// Whether this is the primary RSS source (only one per bangumi)
    /// Episodes from primary RSS can override those from backup RSS
    pub is_primary: bool,
    /// Optional subtitle group name
    pub group: Option<String>,
}

/// Request body for creating a new RSS subscription
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateRss {
    /// Foreign key to bangumi
    pub bangumi_id: i64,
    /// RSS feed URL
    pub url: String,
    /// Whether subscription is enabled (default: true)
    #[serde(default = "default_enabled")]
    pub enabled: bool,
    /// Regex patterns to exclude from matching
    #[serde(default)]
    pub exclude_filters: Vec<String>,
    /// Regex patterns to include (AND logic)
    #[serde(default)]
    pub include_filters: Vec<String>,
    /// Whether this is the primary RSS source (default: false)
    #[serde(default)]
    pub is_primary: bool,
    /// Optional subtitle group name
    #[serde(default)]
    pub group: Option<String>,
}

fn default_enabled() -> bool {
    true
}

/// Request body for updating an RSS subscription
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateRss {
    #[serde(default)]
    pub url: Option<String>,
    #[serde(default)]
    pub enabled: Option<bool>,
    #[serde(default)]
    pub exclude_filters: Option<Vec<String>>,
    #[serde(default)]
    pub include_filters: Option<Vec<String>>,
    /// Whether this is the primary RSS source
    #[serde(default)]
    pub is_primary: Option<bool>,
    /// Optional subtitle group name
    #[serde(default)]
    pub group: Option<String>,
}
