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
