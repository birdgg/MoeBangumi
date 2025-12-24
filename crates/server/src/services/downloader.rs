use std::sync::Arc;
use tokio::sync::RwLock;

use downloader::{AddTorrentOptions, Downloader, DownloaderClient, DownloaderConfig, Result};

use crate::models::DownloaderSettings;
use crate::services::SettingsWatcher;

/// Service that manages the downloader client lifecycle.
/// Subscribes to settings changes and re-authenticates when needed.
pub struct DownloaderService {
    client: Arc<RwLock<Option<DownloaderClient>>>,
}

impl DownloaderService {
    /// Create a new DownloaderService and start watching for settings changes.
    pub fn new(initial_settings: DownloaderSettings, mut watcher: SettingsWatcher) -> Self {
        let client: Arc<RwLock<Option<DownloaderClient>>> = Arc::new(RwLock::new(None));

        // Try to create initial client
        let initial_client = Self::create_client(&initial_settings);
        if let Some(c) = initial_client {
            let client_clone = Arc::clone(&client);
            tokio::spawn(async move {
                Self::authenticate_and_store(c, client_clone).await;
            });
        }

        // Spawn background task to watch for settings changes
        let client_clone = Arc::clone(&client);
        let mut last_settings = initial_settings;

        tokio::spawn(async move {
            while watcher.changed().await.is_ok() {
                let new_settings = watcher.borrow_and_update().downloader.clone();

                // Only reconnect if downloader settings actually changed
                if Self::settings_changed(&last_settings, &new_settings) {
                    tracing::info!("Downloader settings changed, reconnecting...");

                    if let Some(new_client) = Self::create_client(&new_settings) {
                        Self::authenticate_and_store(new_client, Arc::clone(&client_clone)).await;
                    } else {
                        // Clear client if settings are incomplete
                        *client_clone.write().await = None;
                        tracing::warn!("Downloader settings incomplete, client cleared");
                    }

                    last_settings = new_settings;
                }
            }
        });

        Self { client }
    }

    /// Check if downloader settings have changed
    fn settings_changed(old: &DownloaderSettings, new: &DownloaderSettings) -> bool {
        old.downloader_type != new.downloader_type
            || old.url != new.url
            || old.username != new.username
            || old.password != new.password
    }

    /// Create a downloader client from settings (if settings are complete)
    fn create_client(settings: &DownloaderSettings) -> Option<DownloaderClient> {
        // Check if required fields are filled
        if settings.url.is_empty() || settings.username.is_empty() || settings.password.is_empty()
        {
            tracing::debug!("Downloader settings incomplete, skipping client creation");
            return None;
        }

        let config = DownloaderConfig {
            downloader_type: settings.downloader_type,
            url: settings.url.clone(),
            username: Some(settings.username.clone()),
            password: Some(settings.password.clone()),
        };

        match DownloaderClient::from_config(config) {
            Ok(client) => Some(client),
            Err(e) => {
                tracing::error!("Failed to create downloader client: {}", e);
                None
            }
        }
    }

    /// Authenticate client and store it
    async fn authenticate_and_store(
        client: DownloaderClient,
        storage: Arc<RwLock<Option<DownloaderClient>>>,
    ) {
        match client.authenticate().await {
            Ok(()) => {
                tracing::info!(
                    "Downloader ({}) authenticated successfully",
                    client.downloader_type()
                );
                *storage.write().await = Some(client);
            }
            Err(e) => {
                tracing::error!("Failed to authenticate downloader: {}", e);
                *storage.write().await = None;
            }
        }
    }

    /// Check if a downloader client is available
    pub async fn is_available(&self) -> bool {
        self.client.read().await.is_some()
    }

    /// Add a torrent using the current downloader
    pub async fn add_torrent(&self, options: AddTorrentOptions) -> Result<String> {
        let guard = self.client.read().await;
        match guard.as_ref() {
            Some(client) => client.add_torrent(options).await,
            None => Err(downloader::DownloaderError::Config(
                "No downloader configured".into(),
            )),
        }
    }

    /// Get the downloader type name
    pub async fn downloader_type(&self) -> Option<&'static str> {
        let guard = self.client.read().await;
        guard.as_ref().map(|c| c.downloader_type())
    }
}
