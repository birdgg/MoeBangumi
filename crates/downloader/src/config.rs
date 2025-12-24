use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

/// Downloader type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, ToSchema)]
#[serde(rename_all = "lowercase")]
pub enum DownloaderType {
    QBittorrent,
}

/// Configuration for creating a downloader client
#[derive(Debug, Clone)]
pub struct DownloaderConfig {
    /// Downloader type
    pub downloader_type: DownloaderType,
    /// API URL
    pub url: String,
    /// Username (qBittorrent)
    pub username: Option<String>,
    /// Password (qBittorrent)
    pub password: Option<String>,
}

impl DownloaderConfig {
    /// Create config for qBittorrent
    pub fn qbittorrent(
        url: impl Into<String>,
        username: impl Into<String>,
        password: impl Into<String>,
    ) -> Self {
        Self {
            downloader_type: DownloaderType::QBittorrent,
            url: url.into(),
            username: Some(username.into()),
            password: Some(password.into()),
        }
    }
}
