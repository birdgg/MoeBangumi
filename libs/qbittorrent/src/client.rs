use reqwest::Client;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::error::QBittorrentError;

pub struct QBittorrentClient {
    client: Client,
    base_url: Arc<str>,
    sid: RwLock<Option<String>>,
}

impl QBittorrentClient {
    /// Create a new client with cookie support (required for authentication)
    pub fn new(base_url: impl Into<String>) -> Self {
        let client = Client::builder()
            .cookie_store(true)
            .build()
            .expect("Failed to create HTTP client");
        Self::with_client(client, base_url)
    }

    /// Create a client with a shared reqwest client (must have cookie_store enabled)
    pub fn with_client(client: Client, base_url: impl Into<String>) -> Self {
        let base_url = base_url.into();
        let base_url = base_url.trim_end_matches('/');
        Self {
            client,
            base_url: Arc::from(base_url),
            sid: RwLock::new(None),
        }
    }

    pub(crate) fn client(&self) -> &Client {
        &self.client
    }

    pub(crate) fn url(&self, path: &str) -> String {
        format!("{}/api/v2{}", self.base_url, path)
    }

    pub(crate) async fn set_sid(&self, sid: String) {
        *self.sid.write().await = Some(sid);
    }

    pub(crate) async fn get_sid(&self) -> Option<String> {
        self.sid.read().await.clone()
    }

    /// Check if the client has an active session (has SID stored)
    pub async fn is_authenticated(&self) -> bool {
        self.sid.read().await.is_some()
    }

    pub(crate) async fn handle_response(&self, response: reqwest::Response) -> crate::Result<()> {
        let status = response.status();
        if !status.is_success() {
            let message = response.text().await.unwrap_or_default();
            return Err(QBittorrentError::Api {
                status_code: status.as_u16(),
                message,
            });
        }
        Ok(())
    }
}
