use crate::client::QBittorrentClient;
use crate::error::QBittorrentError;
use crate::models::SyncMainData;

impl QBittorrentClient {
    /// Get sync maindata for incremental updates
    /// GET /api/v2/sync/maindata
    ///
    /// # Arguments
    /// * `rid` - Response ID from previous call. Use 0 for initial request to get full data.
    ///           Subsequent requests with the returned rid will get incremental updates.
    pub async fn sync_maindata(&self, rid: i64) -> crate::Result<SyncMainData> {
        let url = self.url("/sync/maindata");

        let mut request = self.client().get(&url).query(&[("rid", rid.to_string())]);

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

        let data = response.json::<SyncMainData>().await?;
        Ok(data)
    }
}
