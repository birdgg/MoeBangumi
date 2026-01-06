use thiserror::Error;

#[derive(Debug, Error)]
pub enum QBittorrentError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("Authentication failed: {0}")]
    Auth(String),

    #[error("API error: {status_code} - {message}")]
    Api { status_code: u16, message: String },

    #[error("Invalid torrent: {0}")]
    InvalidTorrent(String),
}
