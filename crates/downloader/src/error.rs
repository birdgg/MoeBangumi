use thiserror::Error;

#[derive(Debug, Error)]
pub enum DownloaderError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("qBittorrent error: {0}")]
    QBittorrent(#[from] qbittorrent::QBittorrentError),

    #[error("Configuration error: {0}")]
    Config(String),

    #[error("Authentication failed: {0}")]
    Auth(String),

    #[error("Downloader not configured")]
    NotConfigured,
}

pub type Result<T> = std::result::Result<T, DownloaderError>;
