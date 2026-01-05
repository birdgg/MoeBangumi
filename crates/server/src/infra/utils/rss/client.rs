use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use reqwest::Client;

use super::error::RssError;
use super::models::{FetchContext, FetchResult, RssItem, RssSource};
use super::parsers::{parse_mikan_feed, parse_nyaa_feed};

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
    async fn client(&self) -> super::Result<Client> {
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

    /// Fetch and parse an RSS feed (simple version without caching)
    ///
    /// # Arguments
    /// * `source` - The RSS source to fetch from
    ///
    /// # Returns
    /// A vector of parsed RSS items
    ///
    /// # Example
    /// ```ignore
    /// use server::rss::{RssClient, RssSource};
    ///
    /// async fn example() -> server::rss::Result<()> {
    ///     let client = RssClient::with_client(reqwest::Client::new());
    ///     let items = client.fetch(&RssSource::Mikan(
    ///         "https://mikanani.me/RSS/Bangumi?bangumiId=123".into()
    ///     )).await?;
    ///
    ///     for item in items {
    ///         println!("{}", item.title());
    ///     }
    ///     Ok(())
    /// }
    /// ```
    pub async fn fetch(&self, source: &RssSource) -> super::Result<Vec<RssItem>> {
        match self.fetch_conditional(source, None).await? {
            FetchResult::Modified { items, .. } => Ok(items),
            FetchResult::NotModified => Ok(vec![]),
        }
    }

    /// Fetch and parse an RSS feed with conditional request support (ETag/Last-Modified)
    ///
    /// # Arguments
    /// * `source` - The RSS source to fetch from
    /// * `context` - Optional context containing ETag/Last-Modified from previous fetch
    ///
    /// # Returns
    /// A `FetchResult` indicating whether the feed was modified or not
    ///
    /// # Example
    /// ```ignore
    /// use server::rss::{RssClient, RssSource, FetchContext, FetchResult};
    ///
    /// async fn example() -> server::rss::Result<()> {
    ///     let client = RssClient::with_client(reqwest::Client::new());
    ///
    ///     // First fetch (no context)
    ///     let result = client.fetch_conditional(
    ///         &RssSource::Mikan("https://mikanani.me/RSS/Bangumi?bangumiId=123".into()),
    ///         None,
    ///     ).await?;
    ///
    ///     if let FetchResult::Modified { items, etag, last_modified } = result {
    ///         // Store etag and last_modified for next fetch
    ///         let context = FetchContext { etag, last_modified };
    ///
    ///         // Next fetch with context - may return NotModified (HTTP 304)
    ///         let result = client.fetch_conditional(
    ///             &RssSource::Mikan("https://mikanani.me/RSS/Bangumi?bangumiId=123".into()),
    ///             Some(&context),
    ///         ).await?;
    ///     }
    ///     Ok(())
    /// }
    /// ```
    pub async fn fetch_conditional(
        &self,
        source: &RssSource,
        context: Option<&FetchContext>,
    ) -> super::Result<FetchResult> {
        let url = source.url();
        tracing::debug!("Fetching RSS feed from: {}", url);

        let client = self.client().await?;
        let mut request = client.get(url);

        // Add conditional request headers if context is provided
        if let Some(ctx) = context {
            if let Some(etag) = &ctx.etag {
                request = request.header("If-None-Match", etag);
            }
            if let Some(last_modified) = &ctx.last_modified {
                request = request.header("If-Modified-Since", last_modified);
            }
        }

        let response = request.send().await?;
        let status = response.status();

        // Handle 304 Not Modified
        if status == reqwest::StatusCode::NOT_MODIFIED {
            tracing::debug!("RSS feed not modified (HTTP 304)");
            return Ok(FetchResult::NotModified);
        }

        if !status.is_success() {
            return Err(RssError::Parse(format!(
                "HTTP {} when fetching {}",
                status, url
            )));
        }

        // Extract cache headers from response
        let etag = response
            .headers()
            .get("ETag")
            .and_then(|v| v.to_str().ok())
            .map(String::from);
        let last_modified = response
            .headers()
            .get("Last-Modified")
            .and_then(|v| v.to_str().ok())
            .map(String::from);

        let bytes = response.bytes().await?;

        let items = match source {
            RssSource::Mikan(_) => parse_mikan_feed(&bytes)?,
            RssSource::Nyaa(_) => parse_nyaa_feed(&bytes)?,
        };

        tracing::debug!("Parsed {} items from RSS feed", items.len());
        Ok(FetchResult::Modified {
            items,
            etag,
            last_modified,
        })
    }
}
