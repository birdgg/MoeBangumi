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
    /// RSS subscription title: [group] {bangumi} S{season}
    pub title: String,
    /// RSS feed URL
    pub url: String,
    /// Whether subscription is enabled
    pub enabled: bool,
    /// Regex patterns to exclude from matching
    pub exclude_filters: Vec<String>,
    /// Whether this is the primary RSS source (only one per bangumi)
    /// Episodes from primary RSS can override those from backup RSS
    pub is_primary: bool,
    /// Optional subtitle group name
    pub group: Option<String>,

    /// HTTP caching: ETag from last response
    #[serde(skip_serializing)]
    pub etag: Option<String>,
    /// HTTP caching: Last-Modified from last response
    #[serde(skip_serializing)]
    pub last_modified: Option<String>,
    /// Last processed pubDate (ISO 8601)
    #[serde(skip_serializing)]
    pub last_pub_date: Option<String>,
}

/// Request body for creating a new RSS subscription
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateRss {
    /// Foreign key to bangumi
    pub bangumi_id: i64,
    /// RSS subscription title: [group] {bangumi} S{season}
    pub title: String,
    /// RSS feed URL
    pub url: String,
    /// Whether subscription is enabled (default: true)
    #[serde(default = "default_enabled")]
    pub enabled: bool,
    /// Regex patterns to exclude from matching
    #[serde(default)]
    pub exclude_filters: Vec<String>,
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
    /// Whether this is the primary RSS source
    #[serde(default)]
    pub is_primary: Option<bool>,
    /// Optional subtitle group name
    #[serde(default)]
    pub group: Option<String>,
}

/// Format RSS subscription title
/// Format: `[group] {bangumi_title} S{season:02}` or `{bangumi_title} S{season:02}` if group is None
pub fn format_rss_title(bangumi_title: &str, season: i32, group: Option<&str>) -> String {
    match group {
        Some(g) => format!("[{}] {} S{:02}", g, bangumi_title, season),
        None => format!("{} S{:02}", bangumi_title, season),
    }
}
