use serde::{Deserialize, Serialize};

/// Torrent information from qBittorrent
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct TorrentInfo {
    /// Torrent hash
    pub hash: String,
    /// Torrent name
    pub name: String,
    /// Torrent state (downloading, uploading, pausedDL, pausedUP, stalledDL, stalledUP, checkingDL, checkingUP, completed, etc.)
    pub state: String,
    /// Torrent progress (0.0 to 1.0)
    pub progress: f64,
    /// Full path to the torrent's download location
    pub save_path: String,
    /// Torrent total size (bytes)
    pub size: i64,
    /// Amount of data downloaded (bytes)
    pub downloaded: i64,
    /// Torrent ETA (seconds)
    pub eta: i64,
}

impl TorrentInfo {
    /// Check if the torrent download is completed
    ///
    /// A torrent is considered completed when:
    /// - Progress is 100% (>= 1.0)
    /// - State indicates upload-related status (uploading, stalledUP, pausedUP, forcedUP, queuedUP)
    /// - State indicates checking after download (checkingUP)
    pub fn is_completed(&self) -> bool {
        self.progress >= 1.0
            || self.state == "uploading"
            || self.state == "stalledUP"
            || self.state == "pausedUP"
            || self.state == "forcedUP"
            || self.state == "queuedUP"
            || self.state == "checkingUP"
    }
}

/// File information within a torrent
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct TorrentFile {
    /// File index
    pub index: i32,
    /// File name (including relative path)
    pub name: String,
    /// File size (bytes)
    pub size: i64,
    /// File progress (0.0 to 1.0)
    pub progress: f64,
    /// File priority (0 = do not download, 1-7 = priority levels)
    pub priority: i32,
}

impl TorrentFile {
    /// Check if the file download is completed
    pub fn is_completed(&self) -> bool {
        self.progress >= 1.0
    }

    /// Check if this is a video file based on extension
    pub fn is_video(&self) -> bool {
        let video_exts = ["mkv", "mp4", "avi", "mov", "webm", "flv", "m4v", "wmv", "ts"];
        self.name
            .rsplit('.')
            .next()
            .map(|ext| video_exts.contains(&ext.to_lowercase().as_str()))
            .unwrap_or(false)
    }

    /// Get the file extension
    pub fn extension(&self) -> Option<&str> {
        self.name.rsplit('.').next()
    }
}

/// Request to add torrents via URLs
#[derive(Debug, Clone, Default, Serialize)]
pub struct AddTorrentRequest {
    /// URLs separated by newlines (HTTP, HTTPS, magnet links)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub urls: Option<String>,
    /// Download folder
    #[serde(skip_serializing_if = "Option::is_none")]
    pub savepath: Option<String>,
    /// Category for the torrent
    #[serde(skip_serializing_if = "Option::is_none")]
    pub category: Option<String>,
    /// Tags for the torrent (comma-separated)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<String>,
    /// Rename torrent
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rename: Option<String>,
}

impl AddTorrentRequest {
    /// Create a new request with a single URL
    pub fn with_url(url: impl Into<String>) -> Self {
        Self {
            urls: Some(url.into()),
            ..Default::default()
        }
    }

    /// Create a new request with multiple URLs
    pub fn with_urls(urls: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let urls_str = urls.into_iter().map(Into::into).collect::<Vec<_>>().join("\n");
        Self {
            urls: Some(urls_str),
            ..Default::default()
        }
    }

    /// Set the save path
    pub fn savepath(mut self, path: impl Into<String>) -> Self {
        self.savepath = Some(path.into());
        self
    }

    /// Set the category
    pub fn category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    /// Set tags (will be joined with comma)
    pub fn tags(mut self, tags: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let tags_str = tags.into_iter().map(Into::into).collect::<Vec<_>>().join(",");
        self.tags = Some(tags_str);
        self
    }

    /// Add a single tag
    pub fn add_tag(mut self, tag: impl Into<String>) -> Self {
        let tag = tag.into();
        self.tags = Some(match self.tags {
            Some(existing) => format!("{},{}", existing, tag),
            None => tag,
        });
        self
    }

    /// Set the rename (torrent name)
    pub fn rename(mut self, name: impl Into<String>) -> Self {
        self.rename = Some(name.into());
        self
    }
}
