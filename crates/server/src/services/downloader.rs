use std::sync::Arc;
use tokio::sync::RwLock;

use crate::models::DownloaderSettings;
use crate::services::SettingsService;

// Re-export types from downloader crate
pub use downloader::{
    AddTorrentOptions, Downloader, DownloaderClient, DownloaderConfig, DownloaderError,
    DownloaderExt, DownloaderType, ServerState, SyncMainData, SyncTorrentInfo, TorrentFile,
    TorrentInfo,
};

/// Cached client with the settings it was created from
struct CachedClient {
    client: DownloaderClient,
    settings: DownloaderSettings,
}

/// Service that manages the downloader client lifecycle.
/// Lazily creates/rebuilds client when settings change.
/// Automatically retries with re-authentication on auth errors.
pub struct DownloaderService {
    settings: Arc<SettingsService>,
    cached: RwLock<Option<CachedClient>>,
}

impl DownloaderService {
    /// Create a new DownloaderService.
    pub fn new(settings: Arc<SettingsService>) -> Self {
        Self {
            settings,
            cached: RwLock::new(None),
        }
    }

    /// Check if downloader settings have changed
    fn settings_changed(old: &DownloaderSettings, new: &DownloaderSettings) -> bool {
        old.downloader_type != new.downloader_type
            || old.url != new.url
            || old.username != new.username
            || old.password != new.password
    }

    /// Check if settings are complete (all required fields filled)
    fn settings_complete(settings: &DownloaderSettings) -> bool {
        !settings.url.is_empty() && !settings.username.is_empty() && !settings.password.is_empty()
    }

    /// Create a downloader client from settings
    fn create_client(settings: &DownloaderSettings) -> downloader::Result<DownloaderClient> {
        let config = DownloaderConfig {
            downloader_type: settings.downloader_type,
            url: settings.url.clone(),
            username: Some(settings.username.clone()),
            password: Some(settings.password.clone()),
        };
        DownloaderClient::from_config(config)
    }

    /// Check if the error is an authentication error that might be recoverable
    fn is_auth_error(error: &DownloaderError) -> bool {
        match error {
            DownloaderError::Auth(_) => true,
            DownloaderError::QBittorrent(qb_err) => match qb_err {
                qbittorrent::QBittorrentError::Auth(_) => true,
                qbittorrent::QBittorrentError::Api { status_code, .. } => {
                    *status_code == 401 || *status_code == 403
                }
                _ => false,
            },
            _ => false,
        }
    }

    /// Ensure we have a valid client, rebuilding if settings changed.
    async fn ensure_client(&self) -> downloader::Result<()> {
        let current_settings = self.settings.get().downloader;

        // Fast path: check if we have a valid cached client
        {
            let guard = self.cached.read().await;
            if let Some(cached) = guard.as_ref() {
                if !Self::settings_changed(&cached.settings, &current_settings) {
                    return Ok(());
                }
            }
        }

        // Slow path: acquire write lock
        let mut guard = self.cached.write().await;

        // Double-check: re-read settings and check cache again
        // (another thread may have updated while we waited for the write lock)
        let current_settings = self.settings.get().downloader;
        if let Some(cached) = guard.as_ref() {
            if !Self::settings_changed(&cached.settings, &current_settings) {
                return Ok(());
            }
        }

        if !Self::settings_complete(&current_settings) {
            *guard = None;
            return Err(DownloaderError::NotConfigured);
        }

        tracing::info!("Creating/rebuilding downloader client...");
        let client = Self::create_client(&current_settings)?;

        // Login (note: we hold the write lock during authentication,
        // which blocks other operations but ensures consistency)
        client.login().await?;
        tracing::info!(
            "Downloader ({}) authenticated successfully",
            client.downloader_type()
        );

        // Store the new client
        *guard = Some(CachedClient {
            client,
            settings: current_settings,
        });

        Ok(())
    }

    /// Re-authenticate the current client
    async fn reauthenticate(&self) -> downloader::Result<()> {
        let guard = self.cached.read().await;
        if let Some(cached) = guard.as_ref() {
            tracing::info!("Re-authenticating downloader...");
            cached.client.login().await?;
            tracing::info!("Re-authentication successful");
            Ok(())
        } else {
            Err(DownloaderError::NotConfigured)
        }
    }

    /// Check if a downloader client is available
    pub async fn is_available(&self) -> bool {
        self.ensure_client().await.is_ok()
    }

    /// Add a task using the current downloader
    pub async fn add_task(&self, options: AddTorrentOptions) -> downloader::Result<String> {
        // Ensure we have a valid client
        self.ensure_client().await?;

        // Add "rename" tag to options
        let options = options.add_tag("rename");

        // First attempt
        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.add_task(options.clone()).await
        };

        match result {
            Ok(value) => Ok(value),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);

                // Try to re-authenticate and retry once
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.add_task(options).await
            }
            Err(e) => Err(e),
        }
    }

    /// Get the downloader type name
    pub async fn downloader_type(&self) -> Option<&'static str> {
        let guard = self.cached.read().await;
        guard.as_ref().map(|c| c.client.downloader_type())
    }

    /// Get task information by hash
    pub async fn get_task_info(
        &self,
        hash: &str,
    ) -> downloader::Result<Option<TorrentInfo>> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.get_task_info(hash).await
        };

        match result {
            Ok(value) => Ok(value),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.get_task_info(hash).await
            }
            Err(e) => Err(e),
        }
    }

    /// Get files for a specific task
    pub async fn get_task_files(&self, hash: &str) -> downloader::Result<Vec<TorrentFile>> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.get_task_files(hash).await
        };

        match result {
            Ok(value) => Ok(value),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.get_task_files(hash).await
            }
            Err(e) => Err(e),
        }
    }

    /// Rename a file within a torrent
    pub async fn rename_file(
        &self,
        hash: &str,
        old_path: &str,
        new_path: &str,
    ) -> downloader::Result<()> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.rename_file(hash, old_path, new_path).await
        };

        match result {
            Ok(()) => Ok(()),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.rename_file(hash, old_path, new_path).await
            }
            Err(e) => Err(e),
        }
    }

    /// Get all tasks
    pub async fn get_tasks(&self) -> downloader::Result<Vec<TorrentInfo>> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.get_tasks().await
        };

        match result {
            Ok(value) => Ok(value),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.get_tasks().await
            }
            Err(e) => Err(e),
        }
    }

    /// Get tasks with optional filtering
    pub async fn get_tasks_filtered(
        &self,
        filter: Option<&str>,
        tag: Option<&str>,
    ) -> downloader::Result<Vec<TorrentInfo>> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.get_tasks_filtered(filter, tag).await
        };

        match result {
            Ok(value) => Ok(value),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.get_tasks_filtered(filter, tag).await
            }
            Err(e) => Err(e),
        }
    }

    /// Pause task by hashes
    pub async fn pause_task(&self, hashes: &[&str]) -> downloader::Result<()> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.pause_task(hashes).await
        };

        match result {
            Ok(()) => Ok(()),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.pause_task(hashes).await
            }
            Err(e) => Err(e),
        }
    }

    /// Resume task by hashes
    pub async fn resume_task(&self, hashes: &[&str]) -> downloader::Result<()> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.resume_task(hashes).await
        };

        match result {
            Ok(()) => Ok(()),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.resume_task(hashes).await
            }
            Err(e) => Err(e),
        }
    }

    /// Delete task by hashes
    pub async fn delete_task(
        &self,
        hashes: &[&str],
        delete_files: bool,
    ) -> downloader::Result<()> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.delete_task(hashes, delete_files).await
        };

        match result {
            Ok(()) => Ok(()),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.delete_task(hashes, delete_files).await
            }
            Err(e) => Err(e),
        }
    }

    /// Get tasks info for incremental updates
    pub async fn get_tasks_info(&self, rid: i64) -> downloader::Result<SyncMainData> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.get_tasks_info(rid).await
        };

        match result {
            Ok(value) => Ok(value),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.get_tasks_info(rid).await
            }
            Err(e) => Err(e),
        }
    }

    /// Add tags to a torrent
    pub async fn add_tags(&self, hash: &str, tags: &[&str]) -> downloader::Result<()> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.add_tags(hash, tags).await
        };

        match result {
            Ok(()) => Ok(()),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.add_tags(hash, tags).await
            }
            Err(e) => Err(e),
        }
    }

    /// Remove tags from a torrent
    pub async fn remove_tags(&self, hash: &str, tags: &[&str]) -> downloader::Result<()> {
        self.ensure_client().await?;

        let result = {
            let guard = self.cached.read().await;
            let client = guard
                .as_ref()
                .map(|c| &c.client)
                .ok_or(DownloaderError::NotConfigured)?;
            client.remove_tags(hash, tags).await
        };

        match result {
            Ok(()) => Ok(()),
            Err(e) if Self::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.reauthenticate().await?;

                let guard = self.cached.read().await;
                let client = guard
                    .as_ref()
                    .map(|c| &c.client)
                    .ok_or(DownloaderError::NotConfigured)?;
                client.remove_tags(hash, tags).await
            }
            Err(e) => Err(e),
        }
    }
}
