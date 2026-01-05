mod actor;
mod config;
mod handle;
pub mod telegram;

use anyhow::Result;
use async_trait::async_trait;
use thiserror::Error;
use tokio::sync::mpsc;

pub use config::{NotificationConfig, TelegramConfig};
pub use handle::NotificationHandle;

/// NotificationService 类型别名，保持 API 兼容
pub type NotificationService = NotificationHandle;

/// Notification service errors
#[derive(Debug, Error)]
pub enum NotificationError {
    /// Provider is not configured
    #[error("Provider not configured: {0}")]
    NotConfigured(String),

    /// Worker error
    #[error("Worker error: {0}")]
    Worker(#[from] anyhow::Error),
}

/// 通知主题
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Topic {
    Download,
    System,
    Error,
}

impl std::fmt::Display for Topic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Topic::Download => write!(f, "下载"),
            Topic::System => write!(f, "系统"),
            Topic::Error => write!(f, "错误"),
        }
    }
}

/// 通知系统核心 trait
#[async_trait]
pub trait Notifier: Send + Sync {
    /// 发送文本消息
    async fn send_message(&self, text: &str) -> Result<()>;

    /// 发送带格式的消息（Markdown/HTML等）
    async fn send_formatted_message(&self, text: &str, parse_mode: &str) -> Result<()>;

    /// 发送带附件的消息
    async fn send_message_with_attachment(
        &self,
        text: &str,
        attachment: &[u8],
        file_name: &str,
    ) -> Result<()>;

    /// 发送带图片的消息
    ///
    /// `cache_key` is used to cache the file_id for reuse (e.g., poster path).
    /// If the same cache_key is used again, the cached file_id will be used
    /// instead of re-uploading the photo.
    async fn send_photo(
        &self,
        caption: &str,
        photo: &[u8],
        parse_mode: &str,
        cache_key: Option<&str>,
    ) -> Result<()>;
}

use std::sync::Arc;
use super::super::HttpClientService;
use super::super::SettingsService;

/// 创建通知服务（从设置创建，带配置监听）
pub fn create_notification_service(
    settings: Arc<SettingsService>,
    http_client: Arc<HttpClientService>,
) -> NotificationService {
    // 从 settings 创建初始配置
    let initial_config = to_notify_config(&settings.get().notification);

    // 创建通知服务
    let handle = create_notification_service_with_config(initial_config, http_client.get_client());

    // 启动配置监听任务
    spawn_notification_config_watcher(settings, http_client, handle.clone());

    handle
}

/// 创建通知服务（直接传入配置）
pub fn create_notification_service_with_config(
    config: NotificationConfig,
    http_client: reqwest::Client,
) -> NotificationHandle {
    let (sender, receiver) = mpsc::channel(32);

    let actor = actor::NotificationActor::new(config, http_client, receiver);
    tokio::spawn(actor.run());

    NotificationHandle::new(sender)
}

/// 启动配置变化监听任务
fn spawn_notification_config_watcher(
    settings: Arc<SettingsService>,
    http_client: Arc<HttpClientService>,
    handle: NotificationHandle,
) {
    let mut watcher = settings.subscribe();
    let initial_config = watcher.borrow().notification.clone();

    tokio::spawn(async move {
        let mut prev_config = initial_config;
        loop {
            if watcher.changed().await.is_err() {
                break;
            }
            let new_config = watcher.borrow().notification.clone();

            if settings_changed(&prev_config, &new_config) {
                let config = to_notify_config(&new_config);
                handle
                    .update_config(config, http_client.get_client())
                    .await;
            }
            prev_config = new_config;
        }
    });
}

/// 将 server 的配置转换为 notify 的配置
fn to_notify_config(s: &crate::models::NotificationSettings) -> NotificationConfig {
    NotificationConfig {
        enabled: s.enabled,
        telegram: TelegramConfig {
            enabled: s.telegram.enabled,
            bot_token: s.telegram.bot_token.clone(),
            chat_id: s.telegram.chat_id.clone(),
        },
    }
}

/// 检查设置是否变化
fn settings_changed(
    old: &crate::models::NotificationSettings,
    new: &crate::models::NotificationSettings,
) -> bool {
    // Check if enabled state changed
    if old.enabled != new.enabled {
        return true;
    }

    // Check telegram config
    let old_tg = &old.telegram;
    let new_tg = &new.telegram;

    old_tg.enabled != new_tg.enabled
        || old_tg.bot_token != new_tg.bot_token
        || old_tg.chat_id != new_tg.chat_id
}
