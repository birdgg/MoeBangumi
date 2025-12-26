use async_trait::async_trait;
use qbittorrent::{AddTorrentRequest, QBittorrentClient, SyncMainData, TorrentFile, TorrentInfo};
use serde::Serialize;
use url::Url;

use crate::error::{DownloaderError, Result};
use crate::models::AddTorrentOptions;
use crate::traits::{Downloader, DownloaderExt};

/// Preferences for configuring qBittorrent autorun
#[derive(Serialize)]
struct AutorunPreferences {
    autorun_enabled: bool,
    autorun_program: String,
}

/// Preferences for disabling qBittorrent autorun
#[derive(Serialize)]
struct DisableAutorunPreferences {
    autorun_enabled: bool,
}

/// qBittorrent downloader wrapper
pub struct QBittorrentDownloader {
    client: QBittorrentClient,
    username: String,
    password: String,
}

impl QBittorrentDownloader {
    /// Create a new qBittorrent downloader
    pub fn new(
        url: impl Into<String>,
        username: impl Into<String>,
        password: impl Into<String>,
    ) -> Self {
        let client = QBittorrentClient::new(url);
        Self {
            client,
            username: username.into(),
            password: password.into(),
        }
    }
}

#[async_trait]
impl Downloader for QBittorrentDownloader {
    async fn login(&self) -> Result<()> {
        self.client.login(&self.username, &self.password).await?;
        tracing::debug!("qBittorrent authenticated successfully");
        Ok(())
    }

    async fn is_login(&self) -> Result<bool> {
        Ok(self.client.is_authenticated().await)
    }

    async fn get_tasks(&self) -> Result<Vec<TorrentInfo>> {
        let torrents = self.client.get_torrents_info(None).await?;
        Ok(torrents)
    }

    async fn get_tasks_info(&self, rid: i64) -> Result<SyncMainData> {
        let data = self.client.sync_maindata(rid).await?;
        Ok(data)
    }

    async fn add_task(&self, options: AddTorrentOptions) -> Result<String> {
        let mut request = AddTorrentRequest::with_url(&options.url);

        if let Some(path) = options.save_path {
            request = request.savepath(path);
        }

        if let Some(category) = options.category {
            request = request.category(category);
        }

        if !options.tags.is_empty() {
            request = request.tags(options.tags);
        }

        if let Some(rename) = options.rename {
            request = request.rename(rename);
        }

        self.client.add_torrent(request).await?;

        // qBittorrent doesn't return an ID, use URL as identifier
        Ok(options.url)
    }

    async fn pause_task(&self, hashes: &[&str]) -> Result<()> {
        self.client.pause_torrents(hashes).await?;
        Ok(())
    }

    async fn resume_task(&self, hashes: &[&str]) -> Result<()> {
        self.client.resume_torrents(hashes).await?;
        Ok(())
    }

    async fn delete_task(&self, hashes: &[&str], delete_files: bool) -> Result<()> {
        self.client.delete_torrents(hashes, delete_files).await?;
        Ok(())
    }

    async fn add_tags(&self, hash: &str, tags: &[&str]) -> Result<()> {
        self.client.add_tags(&[hash], tags).await?;
        Ok(())
    }

    async fn remove_tags(&self, hash: &str, tags: &[&str]) -> Result<()> {
        self.client.remove_tags(&[hash], tags).await?;
        Ok(())
    }

    async fn get_task_files(&self, hash: &str) -> Result<Vec<TorrentFile>> {
        let files = self.client.get_torrent_files(hash).await?;
        Ok(files)
    }

    async fn rename_file(&self, hash: &str, old_path: &str, new_path: &str) -> Result<()> {
        self.client.rename_file(hash, old_path, new_path).await?;
        Ok(())
    }

    fn downloader_type(&self) -> &'static str {
        "qBittorrent"
    }
}

/// Extended methods for qBittorrent (DownloaderExt implementation)
#[async_trait]
impl DownloaderExt for QBittorrentDownloader {
    async fn get_task_info(&self, hash: &str) -> Result<Option<TorrentInfo>> {
        let infos = self.client.get_torrents_info(Some(&[hash])).await?;
        Ok(infos.into_iter().next())
    }

    async fn configure_autorun(&self, webhook_url: &str) -> Result<()> {
        // Validate and parse the URL to prevent command injection
        let parsed_url = Url::parse(webhook_url).map_err(|e| {
            DownloaderError::Config(format!("Invalid webhook URL '{}': {}", webhook_url, e))
        })?;

        // Only allow HTTP and HTTPS schemes
        if !matches!(parsed_url.scheme(), "http" | "https") {
            return Err(DownloaderError::Config(format!(
                "Webhook URL must use HTTP or HTTPS protocol, got: {}",
                parsed_url.scheme()
            )));
        }

        // Build the full callback URL
        let mut callback_url = parsed_url.clone();
        callback_url.set_path("/api/webhook/torrent-completed");
        callback_url.set_query(Some("name=%N"));
        callback_url.set_query(Some("hash=%I"));

        // The URL is now validated and safe to use
        let autorun_program = format!(r#"curl -X POST "{}""#, callback_url);

        let prefs = AutorunPreferences {
            autorun_enabled: true,
            autorun_program,
        };

        self.client.set_preferences(&prefs).await?;
        tracing::info!(
            "Configured qBittorrent autorun with webhook URL: {}",
            callback_url
        );
        Ok(())
    }

    async fn disable_autorun(&self) -> Result<()> {
        let prefs = DisableAutorunPreferences {
            autorun_enabled: false,
        };

        self.client.set_preferences(&prefs).await?;
        tracing::info!("Disabled qBittorrent autorun");
        Ok(())
    }
}
