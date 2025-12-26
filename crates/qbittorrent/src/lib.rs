mod app;
mod auth;
mod client;
mod error;
pub mod models;
mod sync;
mod torrents;

pub use client::QBittorrentClient;
pub use error::QBittorrentError;
pub use models::{
    AddTorrentRequest, ServerState, SyncMainData, SyncTorrentInfo, TorrentFile, TorrentInfo,
};

pub type Result<T> = std::result::Result<T, QBittorrentError>;
