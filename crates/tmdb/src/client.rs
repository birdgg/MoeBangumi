use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use reqwest::Client;

use crate::error::TmdbError;

const BASE_URL: &str = "https://api.themoviedb.org/3";

/// A function that asynchronously provides an HTTP client.
/// Used for dynamic proxy configuration support.
pub type ClientProvider = Arc<
    dyn Fn() -> Pin<Box<dyn Future<Output = Result<Client, Box<dyn std::error::Error + Send + Sync>>> + Send>>
        + Send
        + Sync,
>;

pub struct TmdbClient {
    client_provider: Option<ClientProvider>,
    static_client: Option<Client>,
    pub(crate) api_key: String,
    pub(crate) lang: String,
}

impl TmdbClient {
    /// Create a TmdbClient with a static reqwest Client.
    /// The client will not be updated if proxy settings change.
    pub fn with_client(client: Client, api_key: impl Into<String>) -> Self {
        Self {
            client_provider: None,
            static_client: Some(client),
            api_key: api_key.into(),
            lang: "zh-CN".to_string(),
        }
    }

    /// Create a TmdbClient with a dynamic client provider.
    /// The provider will be called for each request, allowing for dynamic proxy updates.
    pub fn with_client_provider(provider: ClientProvider, api_key: impl Into<String>) -> Self {
        Self {
            client_provider: Some(provider),
            static_client: None,
            api_key: api_key.into(),
            lang: "zh-CN".to_string(),
        }
    }

    /// Get the HTTP client for making requests.
    pub(crate) async fn client(&self) -> crate::Result<Client> {
        if let Some(provider) = &self.client_provider {
            provider()
                .await
                .map_err(|e| TmdbError::HttpClient(e.to_string()))
        } else if let Some(client) = &self.static_client {
            Ok(client.clone())
        } else {
            Err(TmdbError::HttpClient(
                "No HTTP client configured".to_string(),
            ))
        }
    }

    pub(crate) fn url(&self, path: &str) -> String {
        format!("{}{}", BASE_URL, path)
    }

    pub(crate) async fn handle_response<T: serde::de::DeserializeOwned>(
        &self,
        response: reqwest::Response,
    ) -> crate::Result<T> {
        let status = response.status();
        if !status.is_success() {
            let message = response.text().await.unwrap_or_default();
            return Err(TmdbError::Api {
                status_code: status.as_u16(),
                message,
            });
        }
        Ok(response.json().await?)
    }
}
