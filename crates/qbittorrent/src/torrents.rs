use reqwest::multipart::Form;

use crate::client::QBittorrentClient;
use crate::error::QBittorrentError;
use crate::models::AddTorrentRequest;

impl QBittorrentClient {
    /// Add new torrent(s) via URLs
    /// POST /api/v2/torrents/add
    pub async fn add_torrent(&self, request: AddTorrentRequest) -> crate::Result<()> {
        let urls = request.urls.ok_or_else(|| {
            QBittorrentError::InvalidTorrent("At least one URL must be provided".into())
        })?;

        let url = self.url("/torrents/add");

        let mut form = Form::new().text("urls", urls);

        if let Some(savepath) = request.savepath {
            form = form.text("savepath", savepath);
        }
        if let Some(category) = request.category {
            form = form.text("category", category);
        }
        if let Some(tags) = request.tags {
            form = form.text("tags", tags);
        }

        let mut request = self.client().post(&url).multipart(form);

        // Add SID cookie if authenticated
        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;

        let status = response.status();
        if status.as_u16() == 415 {
            return Err(QBittorrentError::InvalidTorrent(
                "Invalid torrent URL or file".into(),
            ));
        }
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
