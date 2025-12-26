use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

/// Torrent information from qBittorrent
#[derive(Debug, Clone, Deserialize, Serialize, ToSchema)]
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

/// Sync maindata response from qBittorrent
/// Used for incremental updates - only changed fields are included
#[derive(Debug, Clone, Deserialize, Serialize, ToSchema)]
pub struct SyncMainData {
    /// Response ID for incremental updates
    /// Pass this value in subsequent requests to get only changes
    pub rid: i64,
    /// Whether this is a full update (true) or incremental (false)
    #[serde(default)]
    pub full_update: bool,
    /// Torrent data - hash -> partial torrent info
    /// In incremental mode, only changed torrents are included
    #[serde(default)]
    pub torrents: HashMap<String, SyncTorrentInfo>,
    /// List of removed torrent hashes (incremental updates only)
    #[serde(default)]
    pub torrents_removed: Vec<String>,
    /// Server state info
    #[serde(default)]
    pub server_state: Option<ServerState>,
}

/// Partial torrent info for sync API
/// All fields are optional because incremental updates only include changed fields
#[derive(Debug, Clone, Deserialize, Serialize, ToSchema)]
pub struct SyncTorrentInfo {
    /// Torrent name
    #[serde(default)]
    pub name: Option<String>,
    /// Torrent state
    #[serde(default)]
    pub state: Option<String>,
    /// Torrent progress (0.0 to 1.0)
    #[serde(default)]
    pub progress: Option<f64>,
    /// Full path to the torrent's download location
    #[serde(default)]
    pub save_path: Option<String>,
    /// Torrent total size (bytes)
    #[serde(default)]
    pub size: Option<i64>,
    /// Amount of data downloaded (bytes)
    #[serde(default)]
    pub downloaded: Option<i64>,
    /// Torrent ETA (seconds)
    #[serde(default)]
    pub eta: Option<i64>,
    /// Download speed (bytes/s)
    #[serde(default)]
    pub dlspeed: Option<i64>,
    /// Upload speed (bytes/s)
    #[serde(default)]
    pub upspeed: Option<i64>,
    /// Number of seeds
    #[serde(default)]
    pub num_seeds: Option<i64>,
    /// Number of leechers
    #[serde(default)]
    pub num_leechs: Option<i64>,
    /// Share ratio
    #[serde(default)]
    pub ratio: Option<f64>,
    /// Time when torrent was added (Unix timestamp)
    #[serde(default)]
    pub added_on: Option<i64>,
    /// Time when torrent completed (Unix timestamp)
    #[serde(default)]
    pub completion_on: Option<i64>,
    /// Category
    #[serde(default)]
    pub category: Option<String>,
    /// Tags (comma separated)
    #[serde(default)]
    pub tags: Option<String>,
    /// Content path
    #[serde(default)]
    pub content_path: Option<String>,
}

/// Server state from sync maindata
#[derive(Debug, Clone, Deserialize, Serialize, ToSchema)]
pub struct ServerState {
    /// Global download speed (bytes/s)
    #[serde(default)]
    pub dl_info_speed: Option<i64>,
    /// Global upload speed (bytes/s)
    #[serde(default)]
    pub up_info_speed: Option<i64>,
    /// Total downloaded data (bytes)
    #[serde(default)]
    pub dl_info_data: Option<i64>,
    /// Total uploaded data (bytes)
    #[serde(default)]
    pub up_info_data: Option<i64>,
}
