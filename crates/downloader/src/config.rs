use crate::models::DownloaderType;

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

    /// Create config for Transmission
    pub fn transmission(
        url: impl Into<String>,
        username: Option<String>,
        password: Option<String>,
    ) -> Self {
        Self {
            downloader_type: DownloaderType::Transmission,
            url: url.into(),
            username,
            password,
        }
    }
}
