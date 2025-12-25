mod client;
mod config;
mod error;
mod models;
mod qbittorrent_impl;
mod traits;

pub use client::DownloaderClient;
pub use config::DownloaderConfig;
pub use error::DownloaderError;
pub use models::{AddTorrentOptions, DownloaderType};
pub use traits::Downloader;

/// Result type alias for downloader operations
pub type Result<T> = std::result::Result<T, DownloaderError>;
