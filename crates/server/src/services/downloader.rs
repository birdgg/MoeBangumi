use std::sync::Arc;
use tokio::sync::RwLock;

use crate::models::DownloaderSettings;
use crate::services::SettingsService;

/// Macro to execute an operation with automatic retry on auth errors.
macro_rules! with_retry {
    ($self:expr, $client:ident, $op:expr) => {{
        $self.ensure_client().await?;

        let result = {
            let guard = $self.cached.read().await;
            let $client = guard
                .as_ref()
                .ok_or(DownloaderError::NotConfigured)?;
            $op
        };

        match result {
            Ok(value) => Ok(value),
            Err(e) if DownloaderService::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                $self.reauthenticate().await?;

                let guard = $self.cached.read().await;
                let $client = guard
                    .as_ref()
                    .ok_or(DownloaderError::NotConfigured)?;
                $op
            }
            Err(e) => Err(e),
        }
    }};
}

// Re-export types from downloader crate
pub use downloader::{
    AddTaskOptions, Downloader, DownloaderClient, DownloaderConfig, DownloaderError,
    DownloaderType, Task, TaskFile, TaskFilter, TaskStatus,
};

/// Service that manages the downloader client lifecycle.
/// Watches for settings changes and invalidates cache automatically.
/// Lazily creates client on first use after invalidation.
/// Automatically retries with re-authentication on auth errors.
pub struct DownloaderService {
    settings: Arc<SettingsService>,
    cached: Arc<RwLock<Option<DownloaderClient>>>,
}

impl DownloaderService {
    /// Create a new DownloaderService.
    /// Spawns a background task to watch for settings changes.
    pub fn new(settings: Arc<SettingsService>) -> Self {
        let cached = Arc::new(RwLock::new(None));

        // Spawn background task to watch for settings changes
        let cached_clone = Arc::clone(&cached);
        let mut watcher = settings.subscribe();
        let initial_settings = watcher.borrow().downloader.clone();

        tokio::spawn(async move {
            let mut prev_settings = initial_settings;
            loop {
                if watcher.changed().await.is_err() {
                    break;
                }
                let new_settings = watcher.borrow().downloader.clone();

                if Self::settings_changed(&prev_settings, &new_settings) {
                    tracing::info!("Downloader settings changed, invalidating client cache");
                    *cached_clone.write().await = None;
                }
                prev_settings = new_settings;
            }
        });

        Self { settings, cached }
    }

    /// Check if downloader settings have changed (only checks active type's config)
    fn settings_changed(old: &DownloaderSettings, new: &DownloaderSettings) -> bool {
        // Type change always triggers rebuild
        if old.downloader_type != new.downloader_type {
            return true;
        }

        // Check if current downloader's config changed
        match new.downloader_type {
            DownloaderType::QBittorrent => {
                let old_cfg = &old.configs.qbittorrent;
                let new_cfg = &new.configs.qbittorrent;
                old_cfg.url != new_cfg.url
                    || old_cfg.username != new_cfg.username
                    || old_cfg.password != new_cfg.password
            }
            DownloaderType::Transmission => {
                let old_cfg = &old.configs.transmission;
                let new_cfg = &new.configs.transmission;
                old_cfg.url != new_cfg.url
                    || old_cfg.username != new_cfg.username
                    || old_cfg.password != new_cfg.password
            }
        }
    }

    /// Create a downloader client from settings
    fn create_client(settings: &DownloaderSettings) -> downloader::Result<DownloaderClient> {
        let config = settings.get_active_config();
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

    /// Ensure we have a valid client, creating one if needed.
    async fn ensure_client(&self) -> downloader::Result<()> {
        // Fast path: check if we have a cached client
        {
            let guard = self.cached.read().await;
            if guard.is_some() {
                return Ok(());
            }
        }

        // Slow path: acquire write lock and create client
        let mut guard = self.cached.write().await;

        // Double-check after acquiring write lock
        if guard.is_some() {
            return Ok(());
        }

        let current_settings = self.settings.get().downloader;
        if !current_settings.is_active_config_complete() {
            return Err(DownloaderError::NotConfigured);
        }

        tracing::info!("Creating downloader client...");
        let client = Self::create_client(&current_settings)?;

        client.login().await?;
        tracing::info!("Downloader authenticated successfully");

        *guard = Some(client);
        Ok(())
    }

    /// Re-authenticate the current client
    async fn reauthenticate(&self) -> downloader::Result<()> {
        let guard = self.cached.read().await;
        if let Some(client) = guard.as_ref() {
            tracing::info!("Re-authenticating downloader...");
            client.login().await?;
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
    pub async fn add_task(&self, options: AddTaskOptions) -> downloader::Result<String> {
        let options = options.add_tag("rename");
        with_retry!(self, client, client.add_task(options.clone()).await)
    }

    /// Get files for a specific task
    pub async fn get_task_files(&self, hash: &str) -> downloader::Result<Vec<TaskFile>> {
        with_retry!(self, client, client.get_task_files(hash).await)
    }

    /// Get tasks with optional filtering
    pub async fn get_tasks(&self, filter: Option<&TaskFilter>) -> downloader::Result<Vec<Task>> {
        with_retry!(self, client, client.get_tasks(filter).await)
    }

    /// Delete task by IDs
    pub async fn delete_task(&self, ids: &[&str], delete_files: bool) -> downloader::Result<()> {
        with_retry!(self, client, client.delete_task(ids, delete_files).await)
    }

    /// Add tags to a task
    pub async fn add_tags(&self, id: &str, tags: &[&str]) -> downloader::Result<()> {
        with_retry!(self, client, client.add_tags(id, tags).await)
    }

    /// Remove tags from a task
    pub async fn remove_tags(&self, id: &str, tags: &[&str]) -> downloader::Result<()> {
        with_retry!(self, client, client.remove_tags(id, tags).await)
    }

    /// Rename a file in a task
    pub async fn rename_file(
        &self,
        id: &str,
        old_path: &str,
        new_path: &str,
    ) -> downloader::Result<()> {
        with_retry!(
            self,
            client,
            client.rename_file(id, old_path, new_path).await
        )
    }
}
