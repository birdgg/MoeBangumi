use std::sync::Arc;

use downloader::{Downloader, DownloaderClient, DownloaderError};

use crate::services::SettingsService;

/// Actor 内部状态
pub struct DownloaderActorState {
    settings: Arc<SettingsService>,
    client: Option<DownloaderClient>,
}

impl DownloaderActorState {
    pub fn new(settings: Arc<SettingsService>) -> Self {
        Self {
            settings,
            client: None,
        }
    }

    /// 使客户端失效
    pub fn invalidate(&mut self) {
        self.client = None;
        tracing::debug!("Downloader client invalidated due to settings change");
    }

    /// 确保客户端可用（懒初始化）
    pub async fn ensure_client(&mut self) -> Result<&DownloaderClient, DownloaderError> {
        if self.client.is_none() {
            let current_settings = self.settings.get().downloader;
            if !current_settings.is_active_config_complete() {
                return Err(DownloaderError::NotConfigured);
            }

            let config = current_settings.get_active_config();
            let client = DownloaderClient::from_config(config)?;
            client.login().await?;

            self.client = Some(client);
        }

        Ok(self.client.as_ref().unwrap())
    }

    /// 获取客户端引用（假设已初始化）
    pub fn client(&self) -> Option<&DownloaderClient> {
        self.client.as_ref()
    }

    /// 重新认证（不重建客户端）
    pub async fn reauthenticate(&self) -> Result<(), DownloaderError> {
        if let Some(client) = &self.client {
            tracing::info!("Re-authenticating downloader...");
            client.login().await?;
            tracing::info!("Re-authentication successful");
            Ok(())
        } else {
            Err(DownloaderError::NotConfigured)
        }
    }

}
