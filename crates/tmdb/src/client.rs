use reqwest::Client;

use crate::error::TmdbError;

const BASE_URL: &str = "https://api.themoviedb.org/3";

pub struct TmdbClient {
    client: Client,
    pub(crate) api_key: String,
    pub(crate) lang: String,
}

impl TmdbClient {
    pub fn with_client(client: Client, api_key: impl Into<String>) -> Self {
        Self {
            client,
            api_key: api_key.into(),
            lang: "zh-CN".to_string(),
        }
    }

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
