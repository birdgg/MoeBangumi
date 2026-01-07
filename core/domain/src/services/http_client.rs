use std::sync::Arc;

use parking_lot::RwLock;
use reqwest::{Client, Proxy};
use thiserror::Error;

use crate::models::ProxySettings;
use crate::services::SettingsService;

#[derive(Debug, Error)]
pub enum HttpClientError {
    #[error("Failed to build HTTP client: {0}")]
    Build(#[from] reqwest::Error),
    #[error("Invalid proxy configuration: {0}")]
    InvalidProxy(String),
}

/// Service that manages HTTP client with dynamic proxy support.
/// Uses `Proxy::custom` to allow runtime proxy switching without rebuilding the client.
pub struct HttpClientService {
    proxy_url: Arc<RwLock<Option<String>>>,
    client: Client,
}

impl HttpClientService {
    /// Create a new HttpClientService
    ///
    /// The client is created once and proxy changes are reflected immediately
    /// on subsequent requests without needing to rebuild the client.
    pub fn new(settings: Arc<SettingsService>) -> Result<Self, HttpClientError> {
        let initial_proxy = Self::build_proxy_url(&settings.get().proxy);
        let proxy_url = Arc::new(RwLock::new(initial_proxy.clone()));

        // Create proxy with custom closure that reads from shared state
        let proxy_ref = Arc::clone(&proxy_url);
        let proxy = Proxy::custom(move |_url| proxy_ref.read().clone());

        let client = Client::builder()
            .timeout(std::time::Duration::from_secs(30))
            .connect_timeout(std::time::Duration::from_secs(10))
            .proxy(proxy)
            .build()?;

        if initial_proxy.is_some() {
            tracing::info!("HTTP client initialized with proxy");
        } else {
            tracing::debug!("HTTP client initialized without proxy");
        }

        // Spawn background task to watch for settings changes
        let proxy_url_clone = Arc::clone(&proxy_url);
        let mut watcher = settings.subscribe();
        tokio::spawn(async move {
            loop {
                if watcher.changed().await.is_err() {
                    break;
                }
                let new_settings = watcher.borrow().clone();
                let new_proxy = Self::build_proxy_url(&new_settings.proxy);

                let should_update = {
                    let current = proxy_url_clone.read();
                    *current != new_proxy
                };

                if should_update {
                    *proxy_url_clone.write() = new_proxy.clone();
                    if new_proxy.is_some() {
                        tracing::info!("Proxy configuration updated");
                    } else {
                        tracing::info!("Proxy configuration cleared");
                    }
                }
            }
        });

        Ok(Self { proxy_url, client })
    }

    /// Build proxy URL with embedded credentials if provided
    fn build_proxy_url(settings: &ProxySettings) -> Option<String> {
        if settings.url.is_empty() {
            return None;
        }

        // If no credentials, return URL as-is
        if settings.username.is_empty() {
            return Some(settings.url.clone());
        }

        // Parse URL and embed credentials
        // Format: scheme://user:pass@host:port/path
        if let Some(scheme_end) = settings.url.find("://") {
            let scheme = &settings.url[..scheme_end + 3];
            let rest = &settings.url[scheme_end + 3..];
            let credentials = if settings.password.is_empty() {
                settings.username.clone()
            } else {
                format!("{}:{}", settings.username, settings.password)
            };
            Some(format!("{}{}@{}", scheme, credentials, rest))
        } else {
            // No scheme, assume http://
            let credentials = if settings.password.is_empty() {
                settings.username.clone()
            } else {
                format!("{}:{}", settings.username, settings.password)
            };
            Some(format!("http://{}@{}", credentials, settings.url))
        }
    }

    /// Get the HTTP client (sync, always returns the same instance)
    pub fn get_client(&self) -> Client {
        self.client.clone()
    }

    /// Get current proxy URL (for debugging/display)
    pub fn current_proxy(&self) -> Option<String> {
        self.proxy_url.read().clone()
    }
}
