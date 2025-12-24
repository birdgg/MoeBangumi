use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use super::Clearable;

/// Application settings (singleton)
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Settings {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// qBittorrent Web UI URL (e.g., http://localhost:8080)
    pub qb_url: Option<String>,
    /// qBittorrent username
    pub qb_username: Option<String>,
    /// qBittorrent password (plain text)
    pub qb_password: Option<String>,

    /// Global RSS filters (regex patterns to exclude)
    pub global_rss_filters: Vec<String>,
}

/// Request body for updating settings.
/// All fields are optional - only provided fields will be updated.
/// Use `Clearable` for fields that can be explicitly cleared (send null to clear).
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateSettings {
    /// qBittorrent Web UI URL (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub qb_url: Clearable<String>,
    /// qBittorrent username (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub qb_username: Clearable<String>,
    /// qBittorrent password (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub qb_password: Clearable<String>,
    /// Global RSS filters (replaces entire array if provided)
    #[serde(default)]
    pub global_rss_filters: Option<Vec<String>>,
}
