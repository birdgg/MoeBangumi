use std::sync::Arc;

use parking_lot::RwLock;
use reqwest::Client;

use crate::error::TmdbError;

const BASE_URL: &str = "https://api.themoviedb.org/3";

/// Shared API key that can be updated at runtime.
pub type ApiKey = Arc<RwLock<String>>;

pub struct TmdbClient {
    client: Client,
    api_key: ApiKey,
    pub(crate) lang: String,
}

impl TmdbClient {
    /// Create a TmdbClient with a reqwest Client.
    pub fn new(client: Client, api_key: ApiKey) -> Self {
        Self {
            client,
            api_key,
            lang: "zh-CN".to_string(),
        }
    }

    /// Get the current API key
    pub(crate) fn api_key(&self) -> String {
        self.api_key.read().clone()
    }

    /// Get the HTTP client for making requests.
    pub(crate) fn client(&self) -> &Client {
        &self.client
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
            return Err(TmdbError::Api {
                status_code: status.as_u16(),
                message: body,
            });
        }
        let deserializer = &mut serde_json::Deserializer::from_str(&body);
        serde_path_to_error::deserialize(deserializer).map_err(|e| TmdbError::Json {
            path: e.path().to_string(),
            source: e.into_inner(),
        })
    }
}
