use reqwest::multipart::Form;

use crate::client::QBittorrentClient;
use crate::error::QBittorrentError;
use crate::models::{AddTorrentRequest, IntoForm, TorrentFile, TorrentInfo, TorrentInfoRequest};

impl QBittorrentClient {
    /// Add new torrent(s) via URLs
    /// POST /api/v2/torrents/add
    pub async fn add_torrent(&self, request: AddTorrentRequest) -> crate::Result<()> {
        if request.urls.is_none() {
            return Err(QBittorrentError::InvalidTorrent(
                "At least one URL must be provided".into(),
            ));
        }

        let url = self.url("/torrents/add");
        let form = request.into_form();

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

    /// Get torrent list with optional filters
    /// GET /api/v2/torrents/info
    ///
    /// # Arguments
    /// * `request` - Optional request parameters for filtering torrents
    ///
    /// # Example
    /// ```ignore
    /// // Get all torrents
    /// client.get_torrents_info(None).await?;
    ///
    /// // Get downloading torrents
    /// client.get_torrents_info(Some(
    ///     TorrentInfoRequest::new().filter(TorrentFilter::Downloading)
    /// )).await?;
    ///
    /// // Get torrents by category and tag
    /// client.get_torrents_info(Some(
    ///     TorrentInfoRequest::new()
    ///         .category("anime")
    ///         .tag("moe")
    /// )).await?;
    /// ```
    pub async fn get_torrents_info(
        &self,
        request: Option<TorrentInfoRequest>,
    ) -> crate::Result<Vec<TorrentInfo>> {
        let url = self.url("/torrents/info");

        let mut req = self.client().get(&url);

        if let Some(params) = request {
            let mut query: Vec<(&str, String)> = Vec::new();

            if let Some(filter) = params.filter {
                query.push(("filter", filter.to_string()));
            }
            if let Some(category) = params.category {
                query.push(("category", category));
            }
            if let Some(tag) = params.tag {
                query.push(("tag", tag));
            }
            if let Some(hashes) = params.hashes {
                query.push(("hashes", hashes));
            }

            if !query.is_empty() {
                req = req.query(&query);
            }
        }

        if let Some(sid) = self.get_sid().await {
            req = req.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = req.send().await?;

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

    /// Pause torrent(s)
    /// POST /api/v2/torrents/pause
    ///
    /// # Arguments
    /// * `hashes` - Torrent hashes, or `&["all"]` for all torrents
    pub async fn pause_torrents(&self, hashes: &[&str]) -> crate::Result<()> {
        let url = self.url("/torrents/pause");

        let hashes_str = hashes.join("|");
        let form = Form::new().text("hashes", hashes_str);

        let mut request = self.client().post(&url).multipart(form);

        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }

    /// Resume torrent(s)
    /// POST /api/v2/torrents/resume
    ///
    /// # Arguments
    /// * `hashes` - Torrent hashes, or `&["all"]` for all torrents
    pub async fn resume_torrents(&self, hashes: &[&str]) -> crate::Result<()> {
        let url = self.url("/torrents/resume");

        let hashes_str = hashes.join("|");
        let form = Form::new().text("hashes", hashes_str);

        let mut request = self.client().post(&url).multipart(form);

        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }

    /// Delete torrent(s)
    /// POST /api/v2/torrents/delete
    ///
    /// # Arguments
    /// * `hashes` - Torrent hashes, or `&["all"]` for all torrents
    /// * `delete_files` - Whether to delete downloaded files
    pub async fn delete_torrents(&self, hashes: &[&str], delete_files: bool) -> crate::Result<()> {
        let url = self.url("/torrents/delete");

        let hashes_str = hashes.join("|");
        let form = Form::new()
            .text("hashes", hashes_str)
            .text("deleteFiles", delete_files.to_string());

        let mut request = self.client().post(&url).multipart(form);

        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }
}
