use async_trait::async_trait;
use qbittorrent::{TorrentFile, TorrentInfo};

use crate::config::DownloaderConfig;
use crate::error::{DownloaderError, Result};
use crate::models::{AddTorrentOptions, DownloaderType};
use crate::qbittorrent_impl::QBittorrentDownloader;
use crate::traits::Downloader;

/// Unified downloader client (enum dispatch)
pub enum DownloaderClient {
    QBittorrent(QBittorrentDownloader),
}

impl DownloaderClient {
    /// Create a downloader client from configuration
    pub fn from_config(config: DownloaderConfig) -> Result<Self> {
        match config.downloader_type {
            DownloaderType::QBittorrent => {
                let username = config.username.ok_or_else(|| {
                    DownloaderError::Config("qBittorrent requires username".into())
                })?;
                let password = config.password.ok_or_else(|| {
                    DownloaderError::Config("qBittorrent requires password".into())
                })?;

                let downloader = QBittorrentDownloader::new(config.url, username, password);
                Ok(Self::QBittorrent(downloader))
            }
        }
    }
}

/// Implement Downloader trait for DownloaderClient (dispatch to concrete implementations)
#[async_trait]
impl Downloader for DownloaderClient {
    async fn authenticate(&self) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.authenticate().await,
        }
    }

    async fn add_torrent(&self, options: AddTorrentOptions) -> Result<String> {
        match self {
            Self::QBittorrent(d) => d.add_torrent(options).await,
        }
    }

    async fn health_check(&self) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.health_check().await,
        }
    }

    fn downloader_type(&self) -> &'static str {
        match self {
            Self::QBittorrent(d) => d.downloader_type(),
        }
    }

    async fn get_torrent_info(&self, hash: &str) -> Result<Option<TorrentInfo>> {
        match self {
            Self::QBittorrent(d) => d.get_torrent_info(hash).await,
        }
    }

    async fn get_torrent_files(&self, hash: &str) -> Result<Vec<TorrentFile>> {
        match self {
            Self::QBittorrent(d) => d.get_torrent_files(hash).await,
        }
    }
}

/// Downloader-specific methods (not part of the common Downloader trait)
///
/// These methods may not be supported by all downloader implementations.
/// Currently only qBittorrent supports these features.
impl DownloaderClient {
    /// Rename a file within a torrent (qBittorrent only)
    pub async fn rename_file(&self, hash: &str, old_path: &str, new_path: &str) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.rename_file(hash, old_path, new_path).await,
        }
    }

    /// Configure autorun to call a webhook URL when torrents complete (qBittorrent only)
    pub async fn configure_autorun(&self, webhook_url: &str) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.configure_autorun(webhook_url).await,
        }
    }

    /// Disable autorun callback (qBittorrent only)
    pub async fn disable_autorun(&self) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.disable_autorun().await,
        }
    }
}
