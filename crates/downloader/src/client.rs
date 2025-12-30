use async_trait::async_trait;

use crate::config::DownloaderConfig;
use crate::error::{DownloaderError, Result};
use crate::impls::{QBittorrentDownloader, TransmissionDownloader};
use crate::models::{AddTaskOptions, DownloaderType, Task, TaskFile, TaskFilter};
use crate::traits::Downloader;

/// Unified downloader client (enum dispatch).
///
/// This enum provides a single entry point for all downloader implementations.
/// It automatically dispatches method calls to the appropriate implementation
/// based on the configured downloader type.
pub enum DownloaderClient {
    QBittorrent(QBittorrentDownloader),
    Transmission(TransmissionDownloader),
}

impl DownloaderClient {
    /// Create a downloader client from configuration.
    ///
    /// # Errors
    ///
    /// Returns `DownloaderError::Config` if required credentials are missing.
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
            DownloaderType::Transmission => {
                let downloader =
                    TransmissionDownloader::new(config.url, config.username, config.password)?;
                Ok(Self::Transmission(downloader))
            }
        }
    }
}

#[async_trait]
impl Downloader for DownloaderClient {
    async fn login(&self) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.login().await,
            Self::Transmission(d) => d.login().await,
        }
    }

    async fn is_login(&self) -> Result<bool> {
        match self {
            Self::QBittorrent(d) => d.is_login().await,
            Self::Transmission(d) => d.is_login().await,
        }
    }

    async fn add_task(&self, options: AddTaskOptions) -> Result<String> {
        match self {
            Self::QBittorrent(d) => d.add_task(options).await,
            Self::Transmission(d) => d.add_task(options).await,
        }
    }

    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.delete_task(ids, delete_files).await,
            Self::Transmission(d) => d.delete_task(ids, delete_files).await,
        }
    }

    async fn get_tasks(&self, filter: Option<&TaskFilter>) -> Result<Vec<Task>> {
        match self {
            Self::QBittorrent(d) => d.get_tasks(filter).await,
            Self::Transmission(d) => d.get_tasks(filter).await,
        }
    }

    async fn get_task_files(&self, id: &str) -> Result<Vec<TaskFile>> {
        match self {
            Self::QBittorrent(d) => d.get_task_files(id).await,
            Self::Transmission(d) => d.get_task_files(id).await,
        }
    }

    async fn add_tags(&self, id: &str, tags: &[&str]) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.add_tags(id, tags).await,
            Self::Transmission(d) => d.add_tags(id, tags).await,
        }
    }

    async fn remove_tags(&self, id: &str, tags: &[&str]) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.remove_tags(id, tags).await,
            Self::Transmission(d) => d.remove_tags(id, tags).await,
        }
    }

    async fn rename_file(&self, id: &str, old_path: &str, new_path: &str) -> Result<()> {
        match self {
            Self::QBittorrent(d) => d.rename_file(id, old_path, new_path).await,
            Self::Transmission(d) => d.rename_file(id, old_path, new_path).await,
        }
    }
}
