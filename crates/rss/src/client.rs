use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use reqwest::Client;

use crate::error::RssError;
use crate::models::{RssItem, RssSource};
use crate::parsers::{parse_mikan_feed, parse_nyaa_feed};

/// A function that asynchronously provides an HTTP client.
/// Used for dynamic proxy configuration support.
pub type ClientProvider = Arc<
    dyn Fn() -> Pin<Box<dyn Future<Output = Result<Client, Box<dyn std::error::Error + Send + Sync>>> + Send>>
        + Send
        + Sync,
>;

/// RSS feed fetcher client
pub struct RssClient {
    client_provider: Option<ClientProvider>,
    static_client: Option<Client>,
}

impl RssClient {
    /// Create a new RssClient with a static reqwest Client
    pub fn with_client(client: Client) -> Self {
        Self {
            client_provider: None,
            static_client: Some(client),
        }
    }

    /// Create a new RssClient with a dynamic client provider
    pub fn with_client_provider(provider: ClientProvider) -> Self {
        Self {
            client_provider: Some(provider),
            static_client: None,
        }
    }

    /// Get the HTTP client for making requests.
    async fn client(&self) -> crate::Result<Client> {
        if let Some(provider) = &self.client_provider {
            provider()
                .await
                .map_err(|e| RssError::HttpClient(e.to_string()))
        } else if let Some(client) = &self.static_client {
            Ok(client.clone())
        } else {
            Err(RssError::HttpClient(
                "No HTTP client configured".to_string(),
            ))
        }
    }

    /// Fetch and parse an RSS feed
    ///
    /// # Arguments
    /// * `source` - The RSS source to fetch from
    ///
    /// # Returns
    /// A vector of parsed RSS items
    ///
    /// # Example
    /// ```no_run
    /// use rss::{RssClient, RssSource};
    ///
    /// # async fn example() -> rss::Result<()> {
    /// let client = RssClient::with_client(reqwest::Client::new());
    /// let items = client.fetch(&RssSource::Mikan(
    ///     "https://mikanani.me/RSS/Bangumi?bangumiId=123".into()
    /// )).await?;
    ///
    /// for item in items {
    ///     println!("{}", item.title());
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub async fn fetch(&self, source: &RssSource) -> crate::Result<Vec<RssItem>> {
        let url = source.url();
        tracing::debug!("Fetching RSS feed from: {}", url);

        let client = self.client().await?;
        let response = client.get(url).send().await?;
        let status = response.status();

        if !status.is_success() {
            return Err(RssError::Parse(format!(
                "HTTP {} when fetching {}",
                status, url
            )));
        }

        let bytes = response.bytes().await?;

        let items: Vec<RssItem> = match source {
            RssSource::Mikan(_) => {
                let items = parse_mikan_feed(&bytes)?;
                items.into_iter().map(RssItem::Mikan).collect()
            }
            RssSource::Nyaa(_) => {
                let items = parse_nyaa_feed(&bytes)?;
                items.into_iter().map(RssItem::Nyaa).collect()
            }
        };

        tracing::debug!("Parsed {} items from RSS feed", items.len());
        Ok(items)
    }
}
