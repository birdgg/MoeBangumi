use reqwest::Client;

use crate::error::BgmtvError;

const BASE_URL: &str = "https://api.bgm.tv";
pub(crate) const USER_AGENT: &str = "birdgg/moe-rs";

pub struct BgmtvClient {
    client: Client,
}

impl BgmtvClient {
    pub fn with_client(client: Client) -> Self {
        Self { client }
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
            return Err(BgmtvError::Api {
                status_code: status.as_u16(),
                message,
            });
        }
        Ok(response.json().await?)
    }
}
