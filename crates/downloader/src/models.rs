use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

// Re-export torrent info types from qbittorrent crate
pub use qbittorrent::{TorrentFile, TorrentInfo};

/// Downloader type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, ToSchema)]
pub enum DownloaderType {
    #[default]
    #[serde(rename = "qBittorrent")]
    QBittorrent,
}

/// Options for adding a torrent
#[derive(Debug, Clone, Default)]
pub struct AddTorrentOptions {
    /// Torrent URL or magnet link
    pub url: String,
    /// Save path / download directory
    pub save_path: Option<String>,
    /// Category (qBittorrent specific)
    pub category: Option<String>,
    /// Tags/labels
    pub tags: Vec<String>,
    /// Rename torrent (content name)
    pub rename: Option<String>,
}

impl AddTorrentOptions {
    /// Create new options with a URL
    pub fn new(url: impl Into<String>) -> Self {
        Self {
            url: url.into(),
            ..Default::default()
        }
    }

    /// Set the save path
    pub fn save_path(mut self, path: impl Into<String>) -> Self {
        self.save_path = Some(path.into());
        self
    }

    /// Set the category (qBittorrent)
    pub fn category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    /// Set tags
    pub fn tags(mut self, tags: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.tags = tags.into_iter().map(Into::into).collect();
        self
    }

    /// Add a single tag
    pub fn add_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }

    /// Set the rename (torrent content name)
    pub fn rename(mut self, name: impl Into<String>) -> Self {
        self.rename = Some(name.into());
        self
    }
}
