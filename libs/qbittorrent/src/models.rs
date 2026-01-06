use std::collections::HashMap;
use std::fmt;

use reqwest::multipart::Form;
use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

/// Torrent state filter for qBittorrent 5.0+
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TorrentFilter {
    All,
    Downloading,
    Seeding,
    Completed,
    Stopped,
    Active,
    Inactive,
    Running,
    Stalled,
    StalledUploading,
    StalledDownloading,
    Errored,
}

impl fmt::Display for TorrentFilter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            TorrentFilter::All => "all",
            TorrentFilter::Downloading => "downloading",
            TorrentFilter::Seeding => "seeding",
            TorrentFilter::Completed => "completed",
            TorrentFilter::Stopped => "stopped",
            TorrentFilter::Active => "active",
            TorrentFilter::Inactive => "inactive",
            TorrentFilter::Running => "running",
            TorrentFilter::Stalled => "stalled",
            TorrentFilter::StalledUploading => "stalled_uploading",
            TorrentFilter::StalledDownloading => "stalled_downloading",
            TorrentFilter::Errored => "errored",
        };
        write!(f, "{}", s)
    }
}

/// Request parameters for getting torrent list
#[derive(Debug, Clone, Default)]
pub struct TorrentInfoRequest {
    /// Filter by torrent state
    pub filter: Option<TorrentFilter>,
    /// Filter by category (empty string = without category)
    pub category: Option<String>,
    /// Filter by tag (empty string = without tag)
    pub tag: Option<String>,
    /// Filter by torrent hashes (pipe-separated)
    pub hashes: Option<String>,
}

impl TorrentInfoRequest {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn filter(mut self, filter: TorrentFilter) -> Self {
        self.filter = Some(filter);
        self
    }

    pub fn category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    pub fn tag(mut self, tag: impl Into<String>) -> Self {
        self.tag = Some(tag.into());
        self
    }

    pub fn hashes(mut self, hashes: &[&str]) -> Self {
        self.hashes = Some(hashes.join("|"));
        self
    }
}

/// Trait for converting a serializable struct to a multipart form
pub trait IntoForm: Serialize {
    /// Convert this struct to a multipart form by serializing to JSON
    /// and then converting each key-value pair to a form field
    fn into_form(self) -> Form
    where
        Self: Sized,
    {
        let value = serde_json::to_value(&self).expect("Failed to serialize to JSON");
        let obj = value.as_object().expect("Expected JSON object");

        obj.iter().fold(Form::new(), |form, (k, v)| {
            // Skip null values
            if v.is_null() {
                return form;
            }
            // Convert value to string without quotes for string types
            let v_str = match v.as_str() {
                Some(s) => s.to_string(),
                None => v.to_string(),
            };
            form.text(k.to_string(), v_str)
        })
    }
}

/// Torrent information from qBittorrent
#[derive(Debug, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
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
    /// Tags (comma separated)
    #[serde(default)]
    pub tags: String,
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
    /// True if file is seeding/complete
    pub is_seed: bool,
}

impl TorrentFile {
    /// Check if the file download is completed
    pub fn is_completed(&self) -> bool {
        self.progress >= 1.0
    }

    /// Check if this is a video file based on extension
    pub fn is_video(&self) -> bool {
        let video_exts = [
            "mkv", "mp4", "avi", "mov", "webm", "flv", "m4v", "wmv", "ts",
        ];
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
        let urls_str = urls
            .into_iter()
            .map(Into::into)
            .collect::<Vec<_>>()
            .join("\n");
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
        let tags_str = tags
            .into_iter()
            .map(Into::into)
            .collect::<Vec<_>>()
            .join(",");
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

impl IntoForm for AddTorrentRequest {}

/// Sync maindata response from qBittorrent
/// Used for incremental updates - only changed fields are included
#[derive(Debug, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
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
#[derive(Debug, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
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
#[derive(Debug, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
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
