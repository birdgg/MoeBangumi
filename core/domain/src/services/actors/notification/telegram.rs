use super::Notifier;
use anyhow::Result;
use async_trait::async_trait;
use lru::LruCache;
use reqwest::Client;
use std::num::NonZeroUsize;
use std::sync::Arc;
use teloxide::prelude::*;
use teloxide::types::InputFile;
use tokio::sync::Mutex;

/// Maximum number of file_id entries to cache
const FILE_ID_CACHE_SIZE: usize = 500;

/// Telegram 通知实现
pub struct TelegramNotifier {
    bot: Bot,
    chat_id: ChatId,
    /// LRU cache for file_id by cache_key (e.g., poster path)
    /// Limited to FILE_ID_CACHE_SIZE entries to prevent unbounded growth
    file_id_cache: Arc<Mutex<LruCache<String, String>>>,
}

impl TelegramNotifier {
    pub fn new_with_client(client: Client, api_key: &str, chat_id: &str) -> Result<Self> {
        Ok(Self {
            bot: Bot::with_client(api_key, client),
            chat_id: ChatId(chat_id.parse()?),
            file_id_cache: Arc::new(Mutex::new(LruCache::new(
                NonZeroUsize::new(FILE_ID_CACHE_SIZE).unwrap(),
            ))),
        })
    }
}

#[async_trait]
impl Notifier for TelegramNotifier {
    async fn send_message(&self, text: &str) -> Result<()> {
        self.bot
            .send_message(self.chat_id, text)
            .await
            .map(|_| ())
            .map_err(|e| anyhow::anyhow!("Telegram send failed: {}", e))
    }

    async fn send_formatted_message(&self, text: &str, parse_mode: &str) -> Result<()> {
        let mode = parse_mode
            .parse()
            .map_err(|_| anyhow::anyhow!("Invalid parse_mode: {}", parse_mode))?;

        self.bot
            .send_message(self.chat_id, text)
            .parse_mode(mode)
            .await
            .map(|_| ())
            .map_err(|e| anyhow::anyhow!("Telegram send failed: {}", e))
    }

    async fn send_message_with_attachment(
        &self,
        text: &str,
        attachment: &[u8],
        file_name: &str,
    ) -> Result<()> {
        let document = InputFile::memory(attachment.to_owned()).file_name(file_name.to_owned());

        self.bot
            .send_document(self.chat_id, document)
            .caption(text)
            .await
            .map(|_| ())
            .map_err(|e| anyhow::anyhow!("Telegram send failed: {}", e))
    }

    async fn send_photo(
        &self,
        caption: &str,
        photo: &[u8],
        parse_mode: &str,
        cache_key: Option<&str>,
    ) -> Result<()> {
        let mode = parse_mode
            .parse()
            .map_err(|_| anyhow::anyhow!("Invalid parse_mode: {}", parse_mode))?;

        // Check if we have a cached file_id for this cache_key
        if let Some(key) = cache_key {
            let mut cache = self.file_id_cache.lock().await;
            if let Some(file_id) = cache.get(key) {
                // Use cached file_id
                let photo_file = InputFile::file_id(file_id.clone());
                return self
                    .bot
                    .send_photo(self.chat_id, photo_file)
                    .caption(caption)
                    .parse_mode(mode)
                    .await
                    .map(|_| ())
                    .map_err(|e| anyhow::anyhow!("Telegram send photo (cached) failed: {}", e));
            }
        }

        // Upload new photo
        let photo_file = InputFile::memory(photo.to_owned());
        let message = self
            .bot
            .send_photo(self.chat_id, photo_file)
            .caption(caption)
            .parse_mode(mode)
            .await
            .map_err(|e| anyhow::anyhow!("Telegram send photo failed: {}", e))?;

        // Cache the file_id if we have a cache_key
        if let Some(key) = cache_key {
            if let Some(photo_sizes) = message.photo() {
                // Get the largest photo size (last in the array)
                if let Some(largest) = photo_sizes.last() {
                    let mut cache = self.file_id_cache.lock().await;
                    cache.put(key.to_string(), largest.file.id.clone());
                    tracing::debug!("Cached file_id for {}: {}", key, largest.file.id);
                }
            }
        }

        Ok(())
    }
}

