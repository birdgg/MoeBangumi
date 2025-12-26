mod app;
mod auth;
mod client;
mod error;
pub mod models;
mod torrents;

pub use client::QBittorrentClient;
pub use error::QBittorrentError;
pub use models::{AddTorrentRequest, TorrentFile, TorrentInfo};

pub type Result<T> = std::result::Result<T, QBittorrentError>;
