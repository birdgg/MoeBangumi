mod error;

pub use error::NotificationError;
pub use notify::worker::{Topic, Worker as NotificationWorker};
pub use notify::Notifier;

use std::sync::Arc;
use tokio::sync::RwLock;

use crate::models::TelegramConfig;

use super::{HttpClientService, SettingsService};

/// Notification service that manages the notification worker.
///
/// This service wraps the notify crate's Worker and handles:
/// - Dynamic configuration updates from SettingsService
/// - Graceful startup and shutdown
pub struct NotificationService {
    worker: Arc<RwLock<NotificationWorker>>,
    settings: Arc<SettingsService>,
}

impl NotificationService {
    /// Create a new notification service.
    pub fn new(settings: Arc<SettingsService>, http_client: Arc<HttpClientService>) -> Self {
        let mut worker = NotificationWorker::new();

        // Initialize with current settings
        let current_settings = settings.get();
        if let Some(notifier) = Self::create_telegram_notifier(
            &current_settings.notification.telegram,
            http_client.get_client(),
        ) {
            worker.add_notifier(notifier);
        }

        let worker = Arc::new(RwLock::new(worker));

        // Watch for settings changes
        let worker_clone = Arc::clone(&worker);
        let http_clone = Arc::clone(&http_client);
        let mut watcher = settings.subscribe();

        tokio::spawn(async move {
            loop {
                if watcher.changed().await.is_err() {
                    break;
                }
                let new_config = watcher.borrow().notification.telegram.clone();
                if let Some(notifier) =
                    Self::create_telegram_notifier(&new_config, http_clone.get_client())
                {
                    let mut w = worker_clone.write().await;
                    w.set_notifier(notifier);
                }
                tracing::debug!("Notification provider updated due to settings change");
            }
        });

        Self { worker, settings }
    }

    fn create_telegram_notifier(
        config: &TelegramConfig,
        client: reqwest::Client,
    ) -> Option<Box<dyn Notifier>> {
        if config.enabled && !config.bot_token.is_empty() && !config.chat_id.is_empty() {
            match notify::telegram::TelegramNotifier::new_with_client(
                client,
                &config.bot_token,
                &config.chat_id,
            ) {
                Ok(notifier) => Some(Box::new(notifier)),
                Err(e) => {
                    tracing::error!("Failed to create Telegram notifier: {}", e);
                    None
                }
            }
        } else {
            None
        }
    }

    /// Start the notification worker.
    pub async fn start(&self) -> Result<(), NotificationError> {
        let settings = self.settings.get();
        if !settings.notification.enabled {
            tracing::info!("Notification service disabled");
            return Ok(());
        }

        let mut worker = self.worker.write().await;
        worker.spawn().await.map_err(NotificationError::Worker)?;
        tracing::info!("Notification service started");
        Ok(())
    }

    /// Stop the notification worker.
    pub async fn stop(&self) -> Result<(), NotificationError> {
        let worker = self.worker.read().await;
        worker.shutdown().await.map_err(NotificationError::Worker)?;
        tracing::info!("Notification service stopped");
        Ok(())
    }

    /// Send a notification.
    pub async fn notify(
        &self,
        topic: Topic,
        title: impl Into<String>,
        content: impl Into<String>,
    ) -> Result<(), NotificationError> {
        let settings = self.settings.get();
        if !settings.notification.enabled {
            return Ok(());
        }

        let worker = self.worker.read().await;
        worker
            .notify(topic, title, content)
            .await
            .map_err(NotificationError::Worker)
    }

    /// Send an error notification.
    pub async fn notify_error(
        &self,
        title: impl Into<String>,
        error: impl std::fmt::Display,
    ) -> Result<(), NotificationError> {
        self.notify(Topic::Error, title, format!("错误信息: {}", error))
            .await
    }

    /// Send a download notification.
    pub async fn notify_download(
        &self,
        title: impl Into<String>,
        content: impl Into<String>,
    ) -> Result<(), NotificationError> {
        self.notify(Topic::Download, title, content).await
    }

    /// Send a download notification with an image.
    ///
    /// Falls back to text-only notification if image sending fails.
    /// `cache_key` is used to cache the Telegram file_id for reuse (e.g., poster path).
    pub async fn notify_download_with_photo(
        &self,
        title: impl Into<String>,
        content: impl Into<String>,
        photo: &[u8],
        cache_key: Option<&str>,
    ) -> Result<(), NotificationError> {
        let settings = self.settings.get();
        if !settings.notification.enabled {
            return Ok(());
        }

        let title = title.into();
        let content = content.into();

        // Format the caption similar to normal notifications
        let caption = format!(
            "*[下载通知]* {}\n\n{}\n\n_发送时间: {}_",
            title,
            content,
            chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
        );

        let worker = self.worker.read().await;
        worker
            .send_photo_direct(&caption, photo, "Markdown", cache_key)
            .await
            .map_err(NotificationError::Worker)
    }
}
