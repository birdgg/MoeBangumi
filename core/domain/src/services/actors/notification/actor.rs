use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::time::Duration;

use chrono::NaiveDateTime;
use lru::LruCache;
use tokio::sync::{mpsc, Mutex};
use tracing::{debug, error, info};

use super::telegram::TelegramNotifier;
use super::{NotificationConfig, Notifier, Topic};

/// 内部消息类型
pub(crate) enum Message {
    Notify {
        topic: Topic,
        title: String,
        content: String,
    },
    NotifyWithPhoto {
        title: String,
        content: String,
        photo: Vec<u8>,
        cache_key: Option<String>,
    },
    UpdateConfig {
        config: NotificationConfig,
        client: reqwest::Client,
    },
}

/// 通知 Actor
pub(crate) struct NotificationActor {
    config: NotificationConfig,
    http_client: reqwest::Client,
    notifier: Option<Box<dyn Notifier>>,
    dedup_caches: HashMap<Topic, Mutex<LruCache<String, NaiveDateTime>>>,
    receiver: mpsc::Receiver<Message>,
}

impl NotificationActor {
    pub fn new(
        config: NotificationConfig,
        http_client: reqwest::Client,
        receiver: mpsc::Receiver<Message>,
    ) -> Self {
        let notifier = Self::create_notifier(&config, http_client.clone());
        let dedup_caches = Self::init_dedup_caches();

        Self {
            config,
            http_client,
            notifier,
            dedup_caches,
            receiver,
        }
    }

    fn init_dedup_caches() -> HashMap<Topic, Mutex<LruCache<String, NaiveDateTime>>> {
        let mut caches = HashMap::new();
        caches.insert(
            Topic::Download,
            Mutex::new(LruCache::new(NonZeroUsize::new(50).unwrap())),
        );
        caches.insert(
            Topic::System,
            Mutex::new(LruCache::new(NonZeroUsize::new(50).unwrap())),
        );
        caches.insert(
            Topic::Error,
            Mutex::new(LruCache::new(NonZeroUsize::new(2000).unwrap())),
        );
        caches
    }

    fn create_notifier(
        config: &NotificationConfig,
        client: reqwest::Client,
    ) -> Option<Box<dyn Notifier>> {
        if !config.enabled || !config.telegram.enabled {
            return None;
        }

        if config.telegram.bot_token.is_empty() || config.telegram.chat_id.is_empty() {
            return None;
        }

        match TelegramNotifier::new_with_client(
            client,
            &config.telegram.bot_token,
            &config.telegram.chat_id,
        ) {
            Ok(notifier) => Some(Box::new(notifier)),
            Err(e) => {
                error!("Failed to create Telegram notifier: {}", e);
                None
            }
        }
    }

    pub async fn run(mut self) {
        while let Some(msg) = self.receiver.recv().await {
            match msg {
                Message::Notify {
                    topic,
                    title,
                    content,
                } => {
                    self.handle_notify(topic, title, content).await;
                }
                Message::NotifyWithPhoto {
                    title,
                    content,
                    photo,
                    cache_key,
                } => {
                    self.handle_notify_with_photo(title, content, photo, cache_key)
                        .await;
                }
                Message::UpdateConfig { config, client } => {
                    self.handle_update_config(config, client);
                }
            }
        }

        info!("Notification actor stopped");
    }

    async fn handle_notify(&self, topic: Topic, title: String, content: String) {
        if !self.config.enabled || self.notifier.is_none() {
            return;
        }

        if !self.should_send(&topic, &title, &content).await {
            debug!("Message in cooldown, skipping: [{:?}] {}", topic, title);
            return;
        }

        let text = Self::format_message(&topic, &title, &content);

        if let Some(notifier) = &self.notifier {
            if let Err(e) = notifier.send_formatted_message(&text, "Markdown").await {
                error!("Failed to send notification: {}", e);
            }
        }
    }

    async fn handle_notify_with_photo(
        &self,
        title: String,
        content: String,
        photo: Vec<u8>,
        cache_key: Option<String>,
    ) {
        if !self.config.enabled || self.notifier.is_none() {
            return;
        }

        let caption = format!("{}{}", title, content);

        if let Some(notifier) = &self.notifier {
            if let Err(e) = notifier
                .send_photo(&caption, &photo, "Markdown", cache_key.as_deref())
                .await
            {
                error!("Failed to send photo notification: {}", e);
                // Fallback to text
                if let Err(e2) = notifier.send_formatted_message(&caption, "Markdown").await {
                    error!("Failed to send fallback text: {}", e2);
                }
            }
        }
    }

    fn handle_update_config(&mut self, config: NotificationConfig, client: reqwest::Client) {
        self.config = config.clone();
        self.http_client = client.clone();
        self.notifier = Self::create_notifier(&config, client);
        info!("Notification config updated");
    }

    async fn should_send(&self, topic: &Topic, title: &str, content: &str) -> bool {
        let key = Self::hash_message(topic, title, content);
        let cooldown = Self::get_cooldown(topic);

        let cache = self
            .dedup_caches
            .get(topic)
            .expect("BUG: Topic missing from dedup_caches");
        let mut cache = cache.lock().await;

        if let Some(last_time) = cache.get(&key) {
            let now = chrono::Local::now().naive_utc();
            if let Ok(cd) = chrono::Duration::from_std(cooldown) {
                if now.signed_duration_since(*last_time) < cd {
                    return false;
                }
            }
        }

        cache.put(key, chrono::Local::now().naive_utc());
        true
    }

    fn hash_message(topic: &Topic, title: &str, content: &str) -> String {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        topic.hash(&mut hasher);
        title.hash(&mut hasher);
        content.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }

    fn get_cooldown(topic: &Topic) -> Duration {
        match topic {
            Topic::Error => Duration::from_secs(60),
            _ => Duration::from_secs(300),
        }
    }

    fn format_message(_topic: &Topic, title: &str, content: &str) -> String {
        format!("{}{}", title, content)
    }
}
