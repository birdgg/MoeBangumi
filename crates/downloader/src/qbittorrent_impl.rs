use async_trait::async_trait;
use qbittorrent::{AddTorrentRequest, QBittorrentClient};

use crate::error::Result;
use crate::models::AddTorrentOptions;
use crate::traits::Downloader;

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
    async fn authenticate(&self) -> Result<()> {
        self.client.login(&self.username, &self.password).await?;
        tracing::debug!("qBittorrent authenticated successfully");
        Ok(())
    }

    async fn add_torrent(&self, options: AddTorrentOptions) -> Result<String> {
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

        self.client.add_torrent(request).await?;

        // qBittorrent doesn't return an ID, use URL as identifier
        Ok(options.url)
    }

    async fn health_check(&self) -> Result<()> {
        // Try to authenticate as health check
        self.authenticate().await
    }

    fn downloader_type(&self) -> &'static str {
        "qBittorrent"
    }
}
