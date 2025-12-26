use super::DownloaderType;
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

/// Downloader configuration (supports qBittorrent)
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct DownloaderSettings {
    /// Downloader type: qbittorrent
    #[serde(default = "DownloaderSettings::default_type", rename = "type")]
    pub downloader_type: DownloaderType,
    /// Downloader Web UI URL (e.g., http://localhost:8080)
    #[serde(default = "DownloaderSettings::default_url")]
    pub url: String,
    /// Username (qBittorrent)
    #[serde(default)]
    pub username: String,
    /// Password (qBittorrent)
    #[serde(default)]
    pub password: String,
    /// Default save path for downloads
    #[serde(default = "DownloaderSettings::default_save_path")]
    pub save_path: String,
    /// Webhook URL for torrent completion callback (e.g., http://192.168.1.100:3000)
    /// Used to configure qBittorrent's autorun to call back when downloads complete
    #[serde(default)]
    pub webhook_url: String,
}

impl Default for DownloaderSettings {
    fn default() -> Self {
        Self {
            downloader_type: Self::default_type(),
            url: Self::default_url(),
            username: String::new(),
            password: String::new(),
            save_path: Self::default_save_path(),
            webhook_url: String::new(),
        }
    }
}

impl DownloaderSettings {
    fn default_type() -> DownloaderType {
        DownloaderType::QBittorrent
    }

    fn default_url() -> String {
        "http://localhost:8080".to_string()
    }

    fn default_save_path() -> String {
        "/Media/Bangumi".to_string()
    }
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
        vec![
            "720[Pp]".to_string(),
            "\\d-\\d".to_string(),
            "合集".to_string(),
        ]
    }
}

impl Settings {
    /// Merge update data into current settings
    pub fn merge(&self, update: UpdateSettings) -> Self {
        Self {
            downloader: DownloaderSettings {
                downloader_type: update
                    .downloader
                    .as_ref()
                    .and_then(|d| d.downloader_type)
                    .unwrap_or(self.downloader.downloader_type),
                url: update
                    .downloader
                    .as_ref()
                    .map(|d| d.url.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.downloader.url.clone()),
                username: update
                    .downloader
                    .as_ref()
                    .map(|d| d.username.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.downloader.username.clone()),
                password: update
                    .downloader
                    .as_ref()
                    .map(|d| d.password.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.downloader.password.clone()),
                save_path: update
                    .downloader
                    .as_ref()
                    .map(|d| d.save_path.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.downloader.save_path.clone()),
                webhook_url: update
                    .downloader
                    .as_ref()
                    .map(|d| d.webhook_url.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.downloader.webhook_url.clone()),
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
    /// Downloader type: qbittorrent
    #[serde(default, rename = "type")]
    pub downloader_type: Option<DownloaderType>,
    /// Downloader Web UI URL (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub url: Clearable<String>,
    /// Username (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub username: Clearable<String>,
    /// Password (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub password: Clearable<String>,
    /// Default save path for downloads (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub save_path: Clearable<String>,
    /// Webhook URL for torrent completion callback (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub webhook_url: Clearable<String>,
}

/// Request body for updating filter settings
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateFilterSettings {
    /// Global RSS filters (replaces entire array if provided)
    #[serde(default)]
    pub global_rss_filters: Option<Vec<String>>,
}
