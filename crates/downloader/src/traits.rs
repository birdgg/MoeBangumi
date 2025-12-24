use async_trait::async_trait;

use crate::error::Result;
use crate::models::AddTorrentOptions;

/// Unified downloader interface
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
}
