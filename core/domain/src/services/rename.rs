//! Rename processing module.
//!
//! This module handles automatic renaming of downloaded media files
//! to be compatible with Plex/Jellyfin naming standards.

mod bdrip;
mod paths;
mod service;
mod standard;
mod subtitles;

pub use service::RenameService;

// Re-export error types for external use
use thiserror::Error;

/// Error type for rename operations
#[derive(Debug, Error)]
pub enum RenameError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Downloader error: {0}")]
    Downloader(#[from] downloader::DownloaderError),

    #[error("Torrent not found in database: {0}")]
    TorrentNotFound(String),

    #[error("Bangumi not found for torrent: {0}")]
    BangumiNotFound(i64),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, RenameError>;

/// Result of a rename task processing
#[derive(Debug)]
pub struct RenameTaskResult {
    /// Bangumi ID
    pub bangumi_id: i64,
    /// Bangumi title (Chinese)
    pub bangumi_title: String,
    /// Poster URL (local path like /posters/xxx.jpg)
    pub poster_url: Option<String>,
    /// Total episodes for this bangumi (0 = unknown)
    pub total_episodes: i32,
    /// Successfully renamed episode numbers
    pub renamed_episodes: Vec<i32>,
}
