use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use reqwest::Client;

use crate::error::BgmtvError;

const BASE_URL: &str = "https://api.bgm.tv";
pub(crate) const USER_AGENT: &str = "birdgg/moe-bangumi";

/// A function that asynchronously provides an HTTP client.
/// Used for dynamic proxy configuration support.
pub type ClientProvider = Arc<
    dyn Fn() -> Pin<Box<dyn Future<Output = Result<Client, Box<dyn std::error::Error + Send + Sync>>> + Send>>
        + Send
        + Sync,
>;

pub struct BgmtvClient {
    client_provider: Option<ClientProvider>,
    static_client: Option<Client>,
}

impl BgmtvClient {
    /// Create a BgmtvClient with a static reqwest Client.
    pub fn with_client(client: Client) -> Self {
        Self {
            client_provider: None,
            static_client: Some(client),
        }
    }

    /// Create a BgmtvClient with a dynamic client provider.
    pub fn with_client_provider(provider: ClientProvider) -> Self {
        Self {
            client_provider: Some(provider),
            static_client: None,
        }
    }

    /// Get the HTTP client for making requests.
    pub(crate) async fn client(&self) -> crate::Result<Client> {
        if let Some(provider) = &self.client_provider {
            provider()
                .await
                .map_err(|e| BgmtvError::HttpClient(e.to_string()))
        } else if let Some(client) = &self.static_client {
            Ok(client.clone())
        } else {
            Err(BgmtvError::HttpClient(
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
        let body = response.text().await?;
        if !status.is_success() {
            return Err(BgmtvError::Api {
                status_code: status.as_u16(),
                message: body,
            });
        }
        let deserializer = &mut serde_json::Deserializer::from_str(&body);
        serde_path_to_error::deserialize(deserializer).map_err(|e| BgmtvError::Json {
            path: e.path().to_string(),
            source: e.into_inner(),
        })
    }
}
