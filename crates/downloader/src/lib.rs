mod client;
mod config;
mod error;
mod models;
mod traits;

// Implementations
mod qbittorrent_impl;

pub use client::DownloaderClient;
pub use config::{DownloaderConfig, DownloaderType};
pub use error::{DownloaderError, Result};
pub use models::AddTorrentOptions;
pub use traits::Downloader;
