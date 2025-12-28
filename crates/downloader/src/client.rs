use async_trait::async_trait;
use qbittorrent::{SyncMainData, TorrentFile, TorrentInfo};

use crate::config::DownloaderConfig;
use crate::error::{DownloaderError, Result};
use crate::models::{AddTorrentOptions, DownloaderType};
use crate::qbittorrent_impl::QBittorrentDownloader;
use crate::traits::{Downloader, DownloaderExt};

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
    async fn login(&self) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.login().await,
        }
    }

    async fn is_login(&self) -> Result<bool> {
        match self {
            Self::QBittorrent(d) => d.is_login().await,
        }
    }

    async fn get_tasks(&self) -> Result<Vec<TorrentInfo>> {
        self.get_tasks_filtered(None, None).await
    }

    async fn get_tasks_filtered(
        &self,
        filter: Option<&str>,
        tag: Option<&str>,
    ) -> Result<Vec<TorrentInfo>> {
        match self {
            Self::QBittorrent(d) => d.get_tasks_filtered(filter, tag).await,
        }
    }

    async fn get_tasks_info(&self, rid: i64) -> Result<SyncMainData> {
        match self {
            Self::QBittorrent(d) => d.get_tasks_info(rid).await,
        }
    }

    async fn add_task(&self, options: AddTorrentOptions) -> Result<String> {
        match self {
            Self::QBittorrent(d) => d.add_task(options).await,
        }
    }

    async fn pause_task(&self, hashes: &[&str]) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.pause_task(hashes).await,
        }
    }

    async fn resume_task(&self, hashes: &[&str]) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.resume_task(hashes).await,
        }
    }

    async fn delete_task(&self, hashes: &[&str], delete_files: bool) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.delete_task(hashes, delete_files).await,
        }
    }

    async fn add_tags(&self, hash: &str, tags: &[&str]) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.add_tags(hash, tags).await,
        }
    }

    async fn remove_tags(&self, hash: &str, tags: &[&str]) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.remove_tags(hash, tags).await,
        }
    }

    async fn get_task_files(&self, hash: &str) -> Result<Vec<TorrentFile>> {
        match self {
            Self::QBittorrent(d) => d.get_task_files(hash).await,
        }
    }

    async fn rename_file(&self, hash: &str, old_path: &str, new_path: &str) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.rename_file(hash, old_path, new_path).await,
        }
    }

    fn downloader_type(&self) -> &'static str {
        match self {
            Self::QBittorrent(d) => d.downloader_type(),
        }
    }
}

/// Implement DownloaderExt trait for extended features
#[async_trait]
impl DownloaderExt for DownloaderClient {
    async fn get_task_info(&self, hash: &str) -> Result<Option<TorrentInfo>> {
        match self {
            Self::QBittorrent(d) => d.get_task_info(hash).await,
        }
    }

    async fn configure_autorun(&self, webhook_url: &str) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.configure_autorun(webhook_url).await,
        }
    }

    async fn disable_autorun(&self) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.disable_autorun().await,
        }
    }
}
