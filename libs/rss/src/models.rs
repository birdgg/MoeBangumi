use serde::{Deserialize, Serialize};

/// RSS source type with URL
#[derive(Debug, Clone)]
pub enum RssSource {
    /// Mikan RSS feed URL
    Mikan(String),
    /// Nyaa RSS feed URL
    Nyaa(String),
}

impl RssSource {
    /// Get the URL of this RSS source
    pub fn url(&self) -> &str {
        match self {
            RssSource::Mikan(url) | RssSource::Nyaa(url) => url,
        }
    }
}

/// RSS item data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RssItem {
    /// Title of the torrent
    pub title: String,
    /// Direct torrent download URL
    pub torrent_url: String,
    /// BitTorrent info hash
    pub info_hash: String,
    /// Publication date (ISO 8601 format for easy comparison)
    pub pub_date: Option<String>,
}

impl RssItem {
    /// Get the title of the RSS item
    pub fn title(&self) -> &str {
        &self.title
    }

    /// Get the torrent URL
    pub fn torrent_url(&self) -> &str {
        &self.torrent_url
    }

    /// Get the info hash
    pub fn info_hash(&self) -> &str {
        &self.info_hash
    }
}

/// Context for conditional HTTP requests (ETag/Last-Modified)
#[derive(Debug, Clone, Default)]
pub struct FetchContext {
    /// ETag from previous response
    pub etag: Option<String>,
    /// Last-Modified from previous response
    pub last_modified: Option<String>,
}

/// Result of an RSS fetch operation
#[derive(Debug, Clone)]
pub enum FetchResult {
    /// RSS feed has not been modified (HTTP 304)
    NotModified,
    /// RSS feed was fetched successfully
    Modified {
        /// Parsed RSS items
        items: Vec<RssItem>,
        /// ETag from response (if present)
        etag: Option<String>,
        /// Last-Modified from response (if present)
        last_modified: Option<String>,
    },
}
