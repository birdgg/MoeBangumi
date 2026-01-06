use reqwest::Client;

const BASE_URL: &str = "https://api.bgm.tv";
pub(crate) const USER_AGENT: &str = "birdgg/moe-bangumi";

pub struct BgmtvClient {
    client: Client,
}

impl BgmtvClient {
    /// Create a BgmtvClient with a reqwest Client.
    pub fn new(client: Client) -> Self {
        Self { client }
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
            return Err(crate::BgmtvError::Api {
                status_code: status.as_u16(),
                message: body,
            });
        }
        let deserializer = &mut serde_json::Deserializer::from_str(&body);
        serde_path_to_error::deserialize(deserializer).map_err(|e| crate::BgmtvError::Json {
            path: e.path().to_string(),
            source: e.into_inner(),
        })
    }
}
