use rss::RssItem;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct TorrentSearchResult {
    pub title: String,
    pub torrent_url: String,
    pub info_hash: String,
    pub source: TorrentSource,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize, ToSchema)]
#[serde(rename_all = "lowercase")]
pub enum TorrentSource {
    Mikan,
    #[default]
    Nyaa,
}

impl std::fmt::Display for TorrentSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TorrentSource::Mikan => write!(f, "mikan"),
            TorrentSource::Nyaa => write!(f, "nyaa"),
        }
    }
}

impl From<RssItem> for TorrentSearchResult {
    fn from(item: RssItem) -> Self {
        let source = match &item {
            RssItem::Mikan(_) => TorrentSource::Mikan,
            RssItem::Nyaa(_) => TorrentSource::Nyaa,
        };

        Self {
            title: item.title().to_string(),
            torrent_url: item.torrent_url().to_string(),
            info_hash: item.info_hash().to_string(),
            source,
        }
    }
}
