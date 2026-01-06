use serde::Serialize;

use crate::client::QBittorrentClient;
use crate::error::QBittorrentError;

impl QBittorrentClient {
    /// Get default save path
    /// GET /api/v2/app/defaultSavePath
    pub async fn default_save_path(&self) -> crate::Result<String> {
        let url = self.url("/app/defaultSavePath");

        let mut request = self.client().get(&url);

        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;

        let status = response.status();
        if !status.is_success() {
            let message = response.text().await.unwrap_or_default();
            return Err(QBittorrentError::Api {
                status_code: status.as_u16(),
                message,
            });
        }

        Ok(response.text().await?)
    }

    /// Set application preferences
    /// POST /api/v2/app/setPreferences
    ///
    /// # Arguments
    /// * `prefs` - A JSON object containing the preferences to set
    pub async fn set_preferences<T: Serialize>(&self, prefs: &T) -> crate::Result<()> {
        let url = self.url("/app/setPreferences");

        let json_str = serde_json::to_string(prefs).map_err(|e| QBittorrentError::Api {
            status_code: 0,
            message: format!("Failed to serialize preferences: {}", e),
        })?;

        let params = [("json", json_str)];

        let mut request = self.client().post(&url).form(&params);

        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }
}
