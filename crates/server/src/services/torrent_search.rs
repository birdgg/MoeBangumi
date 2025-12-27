use std::sync::Arc;

use rss::{RssClient, RssSource};
use thiserror::Error;

use crate::models::{TorrentSearchResult, TorrentSource};

#[derive(Debug, Error)]
pub enum TorrentSearchError {
    #[error("Search source failed")]
    SourceFailed,
}

pub struct TorrentSearchService {
    rss_client: Arc<RssClient>,
}

impl TorrentSearchService {
    pub fn new(rss_client: Arc<RssClient>) -> Self {
        Self { rss_client }
    }

    /// Search torrents from a specific source
    pub async fn search(
        &self,
        keyword: &str,
        source: TorrentSource,
    ) -> Result<Vec<TorrentSearchResult>, TorrentSearchError> {
        let rss_source = match source {
            TorrentSource::Mikan => {
                let url = format!(
                    "https://mikanani.me/Home/Search?searchstr={}",
                    urlencoding::encode(keyword)
                );
                RssSource::Mikan(url)
            }
            TorrentSource::Nyaa => {
                let url = format!(
                    "https://nyaa.si/?page=rss&q={}&c=0_0&f=0",
                    urlencoding::encode(keyword)
                );
                RssSource::Nyaa(url)
            }
        };

        match self.rss_client.fetch(&rss_source).await {
            Ok(items) => {
                let results = items.into_iter().map(TorrentSearchResult::from).collect();
                Ok(results)
            }
            Err(e) => {
                tracing::warn!("{:?} search failed: {:?}", source, e);
                Err(TorrentSearchError::SourceFailed)
            }
        }
    }
}
