use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use super::Clearable;

/// Application settings stored in TOML file
#[derive(Debug, Clone, Default, Serialize, Deserialize, ToSchema)]
pub struct Settings {
    /// Downloader configuration
    #[serde(default)]
    pub downloader: DownloaderSettings,
    /// Filter configuration
    #[serde(default)]
    pub filter: FilterSettings,
}

/// Downloader (qBittorrent) configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize, ToSchema)]
pub struct DownloaderSettings {
    /// qBittorrent Web UI URL (e.g., http://localhost:8080)
    #[serde(default)]
    pub url: Option<String>,
    /// qBittorrent username
    #[serde(default)]
    pub username: Option<String>,
    /// qBittorrent password (plain text)
    #[serde(default)]
    pub password: Option<String>,
}

/// Filter configuration
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct FilterSettings {
    /// Global RSS filters (regex patterns to exclude)
    #[serde(default = "FilterSettings::default_global_rss_filters")]
    pub global_rss_filters: Vec<String>,
}

impl Default for FilterSettings {
    fn default() -> Self {
        Self {
            global_rss_filters: Self::default_global_rss_filters(),
        }
    }
}

impl FilterSettings {
    fn default_global_rss_filters() -> Vec<String> {
        vec!["720[Pp]".to_string()]
    }
}

impl Settings {
    /// Merge update data into current settings
    pub fn merge(&self, update: UpdateSettings) -> Self {
        Self {
            downloader: DownloaderSettings {
                url: update
                    .downloader
                    .as_ref()
                    .map(|d| d.url.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve(self.downloader.url.clone()),
                username: update
                    .downloader
                    .as_ref()
                    .map(|d| d.username.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve(self.downloader.username.clone()),
                password: update
                    .downloader
                    .as_ref()
                    .map(|d| d.password.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve(self.downloader.password.clone()),
            },
            filter: FilterSettings {
                global_rss_filters: update
                    .filter
                    .and_then(|f| f.global_rss_filters)
                    .unwrap_or_else(|| self.filter.global_rss_filters.clone()),
            },
        }
    }
}

/// Request body for updating settings.
/// All fields are optional - only provided fields will be updated.
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateSettings {
    /// Downloader configuration updates
    #[serde(default)]
    pub downloader: Option<UpdateDownloaderSettings>,
    /// Filter configuration updates
    #[serde(default)]
    pub filter: Option<UpdateFilterSettings>,
}

/// Request body for updating downloader settings
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateDownloaderSettings {
    /// qBittorrent Web UI URL (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub url: Clearable<String>,
    /// qBittorrent username (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub username: Clearable<String>,
    /// qBittorrent password (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub password: Clearable<String>,
}

/// Request body for updating filter settings
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateFilterSettings {
    /// Global RSS filters (replaces entire array if provided)
    #[serde(default)]
    pub global_rss_filters: Option<Vec<String>>,
}
