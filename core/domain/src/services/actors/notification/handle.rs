use tokio::sync::mpsc;

use super::actor::Message;
use super::{NotificationConfig, Topic};

/// 通知服务句柄（对外接口）
#[derive(Clone)]
pub struct NotificationHandle {
    sender: mpsc::Sender<Message>,
}

impl NotificationHandle {
    pub(crate) fn new(sender: mpsc::Sender<Message>) -> Self {
        Self { sender }
    }

    /// 发送通知（fire-and-forget）
    pub fn notify(&self, topic: Topic, title: impl Into<String>, content: impl Into<String>) {
        let sender = self.sender.clone();
        let title = title.into();
        let content = content.into();
        tokio::spawn(async move {
            let _ = sender
                .send(Message::Notify {
                    topic,
                    title,
                    content,
                })
                .await;
        });
    }

    /// 发送错误通知（fire-and-forget）
    pub fn notify_error(&self, title: impl Into<String>, error: impl std::fmt::Display) {
        self.notify(Topic::Error, title, format!("错误信息: {}", error));
    }

    /// 发送下载通知（fire-and-forget）
    pub fn notify_download(&self, title: impl Into<String>, content: impl Into<String>) {
        self.notify(Topic::Download, title, content);
    }

    /// 发送带图片的下载通知（fire-and-forget）
    ///
    /// 这是 `notify_with_photo` 的便捷方法，用于下载完成通知。
    pub fn notify_download_with_photo(
        &self,
        title: impl Into<String>,
        content: impl Into<String>,
        photo: Vec<u8>,
        cache_key: Option<String>,
    ) {
        self.notify_with_photo(title, content, photo, cache_key);
    }

    /// 发送带图片的通知（fire-and-forget）
    pub fn notify_with_photo(
        &self,
        title: impl Into<String>,
        content: impl Into<String>,
        photo: Vec<u8>,
        cache_key: Option<String>,
    ) {
        let sender = self.sender.clone();
        let title = title.into();
        let content = content.into();
        tokio::spawn(async move {
            let _ = sender
                .send(Message::NotifyWithPhoto {
                    title,
                    content,
                    photo,
                    cache_key,
                })
                .await;
        });
    }

    /// 更新配置（触发 notifier 重建）
    pub async fn update_config(&self, config: NotificationConfig, client: reqwest::Client) {
        let _ = self
            .sender
            .send(Message::UpdateConfig { config, client })
            .await;
    }
}
