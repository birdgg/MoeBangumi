use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use crate::{
    models::{BangumiDetail, Episode, SearchResult, Subgroup},
    MikanError, Result,
};
use scraper::{ElementRef, Html, Selector};

const BASE_URL: &str = "https://mikanani.me";

/// A function that asynchronously provides an HTTP client.
/// Used for dynamic proxy configuration support.
pub type ClientProvider = Arc<
    dyn Fn() -> Pin<Box<dyn Future<Output = std::result::Result<reqwest::Client, Box<dyn std::error::Error + Send + Sync>>> + Send>>
        + Send
        + Sync,
>;

pub struct MikanClient {
    client_provider: Option<ClientProvider>,
    static_client: Option<reqwest::Client>,
    base_url: String,
}

impl MikanClient {
    /// Create a MikanClient with a static reqwest Client.
    pub fn new(client: reqwest::Client) -> Self {
        Self {
            client_provider: None,
            static_client: Some(client),
            base_url: BASE_URL.to_string(),
        }
    }

    /// Create a MikanClient with a static reqwest Client and custom base URL.
    pub fn with_base_url(client: reqwest::Client, base_url: impl Into<String>) -> Self {
        Self {
            client_provider: None,
            static_client: Some(client),
            base_url: base_url.into(),
        }
    }

    /// Create a MikanClient with a dynamic client provider.
    pub fn with_client_provider(provider: ClientProvider) -> Self {
        Self {
            client_provider: Some(provider),
            static_client: None,
            base_url: BASE_URL.to_string(),
        }
    }

    /// Get the HTTP client for making requests.
    async fn client(&self) -> Result<reqwest::Client> {
        if let Some(provider) = &self.client_provider {
            provider()
                .await
                .map_err(|e| MikanError::HttpClient(e.to_string()))
        } else if let Some(client) = &self.static_client {
            Ok(client.clone())
        } else {
            Err(MikanError::HttpClient(
                "No HTTP client configured".to_string(),
            ))
        }
    }

    pub async fn fetch_html(&self, path: &str) -> Result<String> {
        let url = format!("{}{}", self.base_url, path);
        let client = self.client().await?;
        let response = client.get(&url).send().await?;
        let html = response.text().await?;
        Ok(html)
    }

    pub async fn search_bangumi(&self, keyword: &str) -> Result<Vec<SearchResult>> {
        let encoded = urlencoding::encode(keyword);
        let html = self
            .fetch_html(&format!("/Home/Search?searchstr={}", encoded))
            .await?;

        let document = Html::parse_document(&html);
        let ul_selector = Selector::parse("ul.list-inline.an-ul")
            .map_err(|e| MikanError::Parse(e.to_string()))?;
        let li_selector = Selector::parse("li").map_err(|e| MikanError::Parse(e.to_string()))?;
        let a_selector = Selector::parse("a").map_err(|e| MikanError::Parse(e.to_string()))?;
        let an_text_selector =
            Selector::parse("div.an-text").map_err(|e| MikanError::Parse(e.to_string()))?;

        let mut results = Vec::new();

        if let Some(ul) = document.select(&ul_selector).next() {
            for li in ul.select(&li_selector) {
                let Some(a) = li.select(&a_selector).next() else {
                    continue;
                };

                let href = a.value().attr("href").unwrap_or_default();
                let id = href.trim_start_matches("/Home/Bangumi/").to_string();

                let name = li
                    .select(&an_text_selector)
                    .next()
                    .and_then(|div| div.value().attr("title"))
                    .map(|s| s.to_string())
                    .unwrap_or_default();

                results.push(SearchResult { id, name });
            }
        }

        Ok(results)
    }

    pub async fn get_bangumi_detail(&self, id: &str) -> Result<BangumiDetail> {
        let html = self.fetch_html(&format!("/Home/Bangumi/{}", id)).await?;

        let document = Html::parse_document(&html);
        let subgroup_text_selector =
            Selector::parse("div.subgroup-text").map_err(|e| MikanError::Parse(e.to_string()))?;
        let a_selector = Selector::parse("a").map_err(|e| MikanError::Parse(e.to_string()))?;
        let tr_selector =
            Selector::parse("tbody tr").map_err(|e| MikanError::Parse(e.to_string()))?;
        let name_selector =
            Selector::parse("a.magnet-link-wrap").map_err(|e| MikanError::Parse(e.to_string()))?;
        let torrent_selector =
            Selector::parse("a.magnet-link").map_err(|e| MikanError::Parse(e.to_string()))?;

        let mut subgroups = Vec::new();

        for subgroup_text in document.select(&subgroup_text_selector) {
            // Parse subgroup info
            let subgroup_id = subgroup_text
                .value()
                .attr("id")
                .unwrap_or_default()
                .to_string();

            let subgroup_name = subgroup_text
                .select(&a_selector)
                .next()
                .map(|a| a.text().collect::<String>().trim().to_string())
                .unwrap_or_default();

            // Find the next sibling episode-table
            let episode_table = subgroup_text
                .next_siblings()
                .filter_map(ElementRef::wrap)
                .find(|el| el.value().has_class("episode-table", scraper::CaseSensitivity::CaseSensitive));

            // Parse episodes
            let mut episodes = Vec::new();
            if let Some(episode_table) = episode_table {
                for tr in episode_table.select(&tr_selector) {
                    let name = tr
                        .select(&name_selector)
                        .next()
                        .map(|a| a.text().collect::<String>().trim().to_string())
                        .unwrap_or_default();

                    let torrent_url = tr
                        .select(&torrent_selector)
                        .next()
                        .and_then(|a| a.value().attr("data-clipboard-text"))
                        .map(|s| s.to_string());

                    if !name.is_empty() {
                        episodes.push(Episode {
                            name,
                            torrent_url,
                            sub_type: None,
                            resolution: None,
                        });
                    }
                }
            }

            let rss_url = format!(
                "{}/RSS/Bangumi?bangumiId={}&subgroupid={}",
                self.base_url, id, subgroup_id
            );

            subgroups.push(Subgroup {
                id: subgroup_id,
                name: subgroup_name,
                rss_url,
                episodes,
            });
        }

        Ok(BangumiDetail { subgroups })
    }
}
