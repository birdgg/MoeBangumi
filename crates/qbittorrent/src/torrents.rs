use reqwest::multipart::Form;

use crate::client::QBittorrentClient;
use crate::error::QBittorrentError;
use crate::models::{AddTorrentRequest, TorrentFile, TorrentInfo};

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
        if let Some(rename) = request.rename {
            form = form.text("rename", rename);
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

    /// Add tags to torrent(s)
    /// POST /api/v2/torrents/addTags
    ///
    /// # Arguments
    /// * `hashes` - Torrent hashes, or `&["all"]` for all torrents
    /// * `tags` - Tags to add
    pub async fn add_tags(&self, hashes: &[&str], tags: &[&str]) -> crate::Result<()> {
        let url = self.url("/torrents/addTags");

        let hashes_str = hashes.join("|");
        let tags_str = tags.join(",");
        let form = Form::new().text("hashes", hashes_str).text("tags", tags_str);

        let mut request = self.client().post(&url).multipart(form);

        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }

    /// Remove tags from torrent(s)
    /// POST /api/v2/torrents/removeTags
    ///
    /// # Arguments
    /// * `hashes` - Torrent hashes, or `&["all"]` for all torrents
    /// * `tags` - Tags to remove. If empty, all tags are removed.
    pub async fn remove_tags(&self, hashes: &[&str], tags: &[&str]) -> crate::Result<()> {
        let url = self.url("/torrents/removeTags");

        let hashes_str = hashes.join("|");
        let tags_str = tags.join(",");
        let form = Form::new().text("hashes", hashes_str).text("tags", tags_str);

        let mut request = self.client().post(&url).multipart(form);

        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }

    /// Get torrent list with optional hash filter
    /// GET /api/v2/torrents/info
    ///
    /// # Arguments
    /// * `hashes` - Optional list of torrent hashes to filter by. If None, returns all torrents.
    pub async fn get_torrents_info(
        &self,
        hashes: Option<&[&str]>,
    ) -> crate::Result<Vec<TorrentInfo>> {
        let url = self.url("/torrents/info");

        let mut request = self.client().get(&url);

        if let Some(hashes) = hashes {
            let hashes_str = hashes.join("|");
            request = request.query(&[("hashes", hashes_str)]);
        }

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

        let torrents = response.json::<Vec<TorrentInfo>>().await?;
        Ok(torrents)
    }

    /// Get files for a specific torrent
    /// GET /api/v2/torrents/files
    ///
    /// # Arguments
    /// * `hash` - The torrent hash
    pub async fn get_torrent_files(&self, hash: &str) -> crate::Result<Vec<TorrentFile>> {
        let url = self.url("/torrents/files");

        let mut request = self.client().get(&url).query(&[("hash", hash)]);

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

        let files = response.json::<Vec<TorrentFile>>().await?;
        Ok(files)
    }

    /// Rename a file in a torrent
    /// POST /api/v2/torrents/renameFile
    ///
    /// # Arguments
    /// * `hash` - The torrent hash
    /// * `old_path` - The old file path (relative to the torrent's root)
    /// * `new_path` - The new file path (relative to the torrent's root)
    pub async fn rename_file(
        &self,
        hash: &str,
        old_path: &str,
        new_path: &str,
    ) -> crate::Result<()> {
        let url = self.url("/torrents/renameFile");

        let form = Form::new()
            .text("hash", hash.to_string())
            .text("oldPath", old_path.to_string())
            .text("newPath", new_path.to_string());

        let mut request = self.client().post(&url).multipart(form);

        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }
}
