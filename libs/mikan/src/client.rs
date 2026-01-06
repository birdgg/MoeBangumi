use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, LazyLock};
use std::time::Duration;

use crate::{
    models::{BangumiDetail, Episode, Season, SeasonalBangumi, SearchResult, Subgroup},
    MikanError, Result,
};
use regex::Regex;
use reqwest_middleware::{ClientBuilder, ClientWithMiddleware};
use reqwest_retry::{policies::ExponentialBackoff, RetryTransientMiddleware};
use scraper::{ElementRef, Html, Selector};

// Static regex for BGM.tv ID parsing
static BGMTV_ID_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"bgm\.tv/subject/(\d+)").expect("Invalid regex pattern"));

// Static selectors for search_bangumi
static SEARCH_UL_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("ul.list-inline.an-ul").expect("Invalid selector"));
static SEARCH_LI_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("li").expect("Invalid selector"));
static SEARCH_A_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("a").expect("Invalid selector"));
static SEARCH_AN_TEXT_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("div.an-text").expect("Invalid selector"));

// Static selectors for get_bangumi_detail
static DETAIL_BGMTV_SELECTOR: LazyLock<Selector> = LazyLock::new(|| {
    Selector::parse("div.leftbar-container .bangumi-info a[href*=\"bgm.tv/subject/\"]")
        .expect("Invalid selector")
});
static DETAIL_SUBGROUP_TEXT_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("div.subgroup-text").expect("Invalid selector"));
static DETAIL_A_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("a").expect("Invalid selector"));
static DETAIL_TR_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("tbody tr").expect("Invalid selector"));
static DETAIL_NAME_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("a.magnet-link-wrap").expect("Invalid selector"));
static DETAIL_TORRENT_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("a.magnet-link").expect("Invalid selector"));

// Static selectors for get_seasonal_bangumi_list
static SEASONAL_A_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("a[href^='/Home/Bangumi/']").expect("Invalid selector"));
static SEASONAL_SK_BANGUMI_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("div.sk-bangumi").expect("Invalid selector"));
static SEASONAL_POSTER_SELECTOR: LazyLock<Selector> =
    LazyLock::new(|| Selector::parse("span.js-expand_bangumi[data-src]").expect("Invalid selector"));

const BASE_URL: &str = "https://mikanani.me";

/// Default retry configuration
const DEFAULT_MAX_RETRIES: u32 = 3;
const DEFAULT_MIN_RETRY_INTERVAL: Duration = Duration::from_millis(500);
const DEFAULT_MAX_RETRY_INTERVAL: Duration = Duration::from_secs(5);

/// A function that asynchronously provides an HTTP client.
/// Used for dynamic proxy configuration support.
pub type ClientProvider = Arc<
    dyn Fn() -> Pin<Box<dyn Future<Output = std::result::Result<reqwest::Client, Box<dyn std::error::Error + Send + Sync>>> + Send>>
        + Send
        + Sync,
>;

/// Wrap a reqwest Client with retry middleware
fn wrap_with_retry(client: reqwest::Client) -> ClientWithMiddleware {
    let retry_policy = ExponentialBackoff::builder()
        .retry_bounds(DEFAULT_MIN_RETRY_INTERVAL, DEFAULT_MAX_RETRY_INTERVAL)
        .build_with_max_retries(DEFAULT_MAX_RETRIES);

    ClientBuilder::new(client)
        .with(RetryTransientMiddleware::new_with_policy(retry_policy))
        .build()
}

pub struct MikanClient {
    client_provider: Option<ClientProvider>,
    static_client: Option<ClientWithMiddleware>,
    base_url: String,
}

impl MikanClient {
    /// Create a MikanClient with a static reqwest Client.
    /// Automatically wraps with retry middleware (3 retries with exponential backoff).
    pub fn new(client: reqwest::Client) -> Self {
        Self {
            client_provider: None,
            static_client: Some(wrap_with_retry(client)),
            base_url: BASE_URL.to_string(),
        }
    }

    /// Create a MikanClient with a static reqwest Client and custom base URL.
    /// Automatically wraps with retry middleware.
    pub fn with_base_url(client: reqwest::Client, base_url: impl Into<String>) -> Self {
        Self {
            client_provider: None,
            static_client: Some(wrap_with_retry(client)),
            base_url: base_url.into(),
        }
    }

    /// Create a MikanClient with a dynamic client provider.
    /// Each request will get a fresh client from the provider, wrapped with retry middleware.
    pub fn with_client_provider(provider: ClientProvider) -> Self {
        Self {
            client_provider: Some(provider),
            static_client: None,
            base_url: BASE_URL.to_string(),
        }
    }

    /// Get the HTTP client for making requests.
    async fn client(&self) -> Result<ClientWithMiddleware> {
        if let Some(provider) = &self.client_provider {
            let client = provider()
                .await
                .map_err(|e| MikanError::HttpClient(e.to_string()))?;
            Ok(wrap_with_retry(client))
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

        let mut results = Vec::new();

        if let Some(ul) = document.select(&SEARCH_UL_SELECTOR).next() {
            for li in ul.select(&SEARCH_LI_SELECTOR) {
                let Some(a) = li.select(&SEARCH_A_SELECTOR).next() else {
                    continue;
                };

                let href = a.value().attr("href").unwrap_or_default();
                let id = href.trim_start_matches("/Home/Bangumi/").to_string();

                let name = li
                    .select(&SEARCH_AN_TEXT_SELECTOR)
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

        // Parse bgmtv_id from leftbar-container .bangumi-info a[href*="bgm.tv/subject/"]
        let bgmtv_id = document
            .select(&DETAIL_BGMTV_SELECTOR)
            .next()
            .and_then(|a| a.value().attr("href"))
            .and_then(|href| {
                href.split("/subject/")
                    .nth(1)
                    .and_then(|id_str| id_str.parse::<i64>().ok())
            });

        let mut subgroups = Vec::new();

        for subgroup_text in document.select(&DETAIL_SUBGROUP_TEXT_SELECTOR) {
            // Parse subgroup info
            let subgroup_id = subgroup_text
                .value()
                .attr("id")
                .unwrap_or_default()
                .to_string();

            let subgroup_name = subgroup_text
                .select(&DETAIL_A_SELECTOR)
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
                for tr in episode_table.select(&DETAIL_TR_SELECTOR) {
                    let name = tr
                        .select(&DETAIL_NAME_SELECTOR)
                        .next()
                        .map(|a| a.text().collect::<String>().trim().to_string())
                        .unwrap_or_default();

                    let torrent_url = tr
                        .select(&DETAIL_TORRENT_SELECTOR)
                        .next()
                        .and_then(|a| a.value().attr("data-clipboard-text"))
                        .map(|s| s.to_string());

                    if !name.is_empty() {
                        episodes.push(Episode {
                            name,
                            torrent_url,
                            subtitle_languages: vec![],
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

        Ok(BangumiDetail { bgmtv_id, subgroups })
    }

    /// Get the list of bangumi for a specific season.
    /// URL: /Home/BangumiCoverFlowByDayOfWeek?year={year}&seasonStr={season_chinese}
    /// Returns bangumi with air_week (0=Sunday, 1=Monday, ..., 6=Saturday).
    pub async fn get_seasonal_bangumi_list(
        &self,
        year: i32,
        season: Season,
    ) -> Result<Vec<SeasonalBangumi>> {
        let season_str = urlencoding::encode(season.to_chinese());
        let html = self
            .fetch_html(&format!(
                "/Home/BangumiCoverFlowByDayOfWeek?year={}&seasonStr={}",
                year, season_str
            ))
            .await?;

        let document = Html::parse_document(&html);

        // Build a map of mikan_id -> poster_url from span elements
        let mut poster_map: std::collections::HashMap<String, String> = std::collections::HashMap::new();
        for span in document.select(&SEASONAL_POSTER_SELECTOR) {
            if let Some(bangumi_id) = span.value().attr("data-bangumiid") {
                if let Some(data_src) = span.value().attr("data-src") {
                    // Remove query params from URL
                    let path = data_src.split('?').next().unwrap_or(data_src);
                    // Convert relative URL to absolute
                    let full_url = if path.starts_with('/') {
                        format!("{}{}", self.base_url, path)
                    } else {
                        path.to_string()
                    };
                    poster_map.insert(bangumi_id.to_string(), full_url);
                }
            }
        }

        let mut results = Vec::new();
        let mut seen_ids = std::collections::HashSet::new();

        // Iterate over each sk-bangumi div and get dayofweek
        for sk_bangumi in document.select(&SEASONAL_SK_BANGUMI_SELECTOR) {
            let dayofweek_str = sk_bangumi.value().attr("data-dayofweek").unwrap_or("");
            let air_week: i32 = match dayofweek_str.parse() {
                Ok(d) if d < 7 => d,
                _ => continue, // Skip dayofweek 7, 8 or invalid (special/non-weekly items)
            };

            for a in sk_bangumi.select(&SEASONAL_A_SELECTOR) {
                let href = a.value().attr("href").unwrap_or_default();
                let mikan_id = href.trim_start_matches("/Home/Bangumi/").to_string();

                if mikan_id.is_empty() || seen_ids.contains(&mikan_id) {
                    continue;
                }
                seen_ids.insert(mikan_id.clone());

                let name = a
                    .value()
                    .attr("title")
                    .map(|s| s.to_string())
                    .or_else(|| {
                        let text = a.text().collect::<String>();
                        let trimmed = text.trim();
                        if trimmed.is_empty() {
                            None
                        } else {
                            Some(trimmed.to_string())
                        }
                    })
                    .unwrap_or_default();

                if !name.is_empty() {
                    let poster_url = poster_map.get(&mikan_id).cloned();
                    results.push(SeasonalBangumi { mikan_id, name, air_week, poster_url });
                }
            }
        }

        Ok(results)
    }

    /// Get the BGM.tv ID from a bangumi detail page.
    /// Parses the page for a link to bgm.tv/subject/{id}.
    pub async fn get_bangumi_bgmtv_id(&self, mikan_id: &str) -> Result<Option<i64>> {
        let html = self
            .fetch_html(&format!("/Home/Bangumi/{}", mikan_id))
            .await?;

        if let Some(caps) = BGMTV_ID_RE.captures(&html) {
            if let Some(id_match) = caps.get(1) {
                if let Ok(id) = id_match.as_str().parse::<i64>() {
                    return Ok(Some(id));
                }
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_get_bangumi_detail_bgmtv_id() {
        let client = MikanClient::new(reqwest::Client::new());
        let detail = client.get_bangumi_detail("3742").await.unwrap();
        assert_eq!(detail.bgmtv_id, Some(524195));
        assert!(!detail.subgroups.is_empty());
    }

    #[tokio::test]
    async fn test_get_seasonal_bangumi_list_2025_fall() {
        let client = MikanClient::new(reqwest::Client::new());
        let results = client
            .get_seasonal_bangumi_list(2025, Season::Fall)
            .await
            .unwrap();

        let weekday_names = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
        println!("2025 Fall Bangumi List ({} items):", results.len());
        for bangumi in &results {
            println!("  - {} (mikan_id: {}, air_week: {} ({}), poster: {})",
                bangumi.name,
                bangumi.mikan_id,
                bangumi.air_week,
                weekday_names[bangumi.air_week as usize],
                bangumi.poster_url.as_deref().unwrap_or("N/A")
            );
        }
        assert!(!results.is_empty());
        // Check that at least some posters were found
        let with_poster = results.iter().filter(|b| b.poster_url.is_some()).count();
        println!("\n{} out of {} have poster URLs", with_poster, results.len());
    }
}
