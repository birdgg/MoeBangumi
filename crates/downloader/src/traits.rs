use async_trait::async_trait;

use crate::error::Result;
use crate::models::{AddTorrentOptions, TorrentFile, TorrentInfo};

/// Unified downloader interface
///
/// This trait defines the common operations that all downloaders must support.
/// Downloader-specific features (like qBittorrent's autorun) should be implemented
/// on the concrete types directly.
#[async_trait]
pub trait Downloader: Send + Sync {
    /// Authenticate with the downloader (if required)
    async fn authenticate(&self) -> Result<()>;

    /// Add a torrent
    ///
    /// Returns an identifier for the added torrent (varies by implementation)
    async fn add_torrent(&self, options: AddTorrentOptions) -> Result<String>;

    /// Check if the downloader is reachable and credentials are valid
    async fn health_check(&self) -> Result<()>;

    /// Get the downloader type name (for logging)
    fn downloader_type(&self) -> &'static str;

    /// Get torrent information by hash
    ///
    /// Returns None if the torrent is not found
    async fn get_torrent_info(&self, hash: &str) -> Result<Option<TorrentInfo>>;

    /// Get files for a specific torrent
    async fn get_torrent_files(&self, hash: &str) -> Result<Vec<TorrentFile>>;
}
