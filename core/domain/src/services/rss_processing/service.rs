//! Generic implementation of RssProcessingService using trait bounds.
//!
//! This module contains the core RSS processing logic with dependency injection
//! support through trait bounds. The generic service can work with any implementation
//! of the required traits, enabling easy testing with mock implementations.

use futures::stream::{self, StreamExt};
use parser::{ParseResult, Parser};
use regex::Regex;
use rss::{FetchContext, FetchResult, RssItem, RssSource};
use std::collections::{HashMap, HashSet};

use crate::models::{Bangumi, CreateTorrent, Rss, Torrent};
use crate::services::washing::WashParams;
use washing::{ComparableTorrent, PriorityCalculator};

use super::traits::*;

/// Maximum number of RSS feeds to fetch concurrently
const RSS_FETCH_CONCURRENCY: usize = 5;

/// Context for RSS processing (avoids repeated parameter passing)
struct ProcessingContext {
    bangumi: Bangumi,
}

/// Lookup structures for existing torrents (O(1) access)
struct TorrentLookup {
    existing_hashes: HashSet<String>,
    episodes_map: HashMap<i32, Vec<Torrent>>,
}

impl TorrentLookup {
    fn from_torrents(torrents: Vec<Torrent>) -> Self {
        let mut existing_hashes = HashSet::new();
        let mut episodes_map: HashMap<i32, Vec<Torrent>> = HashMap::new();

        for torrent in torrents {
            existing_hashes.insert(torrent.info_hash.clone());
            if let Some(ep) = torrent.episode_number {
                episodes_map.entry(ep).or_default().push(torrent);
            }
        }

        Self {
            existing_hashes,
            episodes_map,
        }
    }
}

/// Generic RSS processing service with dependency injection.
///
/// Type parameters:
/// - `B`: Bangumi repository
/// - `R`: RSS repository
/// - `T`: Torrent repository
/// - `F`: RSS fetcher
/// - `D`: Downloader
/// - `W`: Washing service
/// - `S`: Settings provider
pub struct GenericRssProcessingService<B, R, T, F, D, W, S> {
    bangumi_repo: B,
    pub(crate) rss_repo: R,
    torrent_repo: T,
    rss_fetcher: F,
    downloader: D,
    washing: W,
    settings: S,
    parser: Parser,
}

impl<B: Clone, R: Clone, T: Clone, F: Clone, D: Clone, W: Clone, S: Clone> Clone
    for GenericRssProcessingService<B, R, T, F, D, W, S>
{
    fn clone(&self) -> Self {
        Self {
            bangumi_repo: self.bangumi_repo.clone(),
            rss_repo: self.rss_repo.clone(),
            torrent_repo: self.torrent_repo.clone(),
            rss_fetcher: self.rss_fetcher.clone(),
            downloader: self.downloader.clone(),
            washing: self.washing.clone(),
            settings: self.settings.clone(),
            parser: Parser::new(),
        }
    }
}

impl<B, R, T, F, D, W, S> GenericRssProcessingService<B, R, T, F, D, W, S>
where
    B: RssBangumiRepository,
    R: RssRssRepository,
    T: RssTorrentRepository,
    F: RssFetcher,
    D: RssDownloader,
    W: RssWashingService,
    S: RssSettingsProvider,
{
    /// Create a new generic RSS processing service with injected dependencies.
    ///
    /// This constructor is used when you need to inject custom implementations
    /// (e.g., for testing). For production use, prefer `RssProcessingService::new()`.
    pub fn with_deps(
        bangumi_repo: B,
        rss_repo: R,
        torrent_repo: T,
        rss_fetcher: F,
        downloader: D,
        washing: W,
        settings: S,
    ) -> Self {
        Self {
            bangumi_repo,
            rss_repo,
            torrent_repo,
            rss_fetcher,
            downloader,
            washing,
            settings,
            parser: Parser::new(),
        }
    }

    /// Process a single RSS subscription synchronously.
    ///
    /// This method fetches the RSS feed, parses items, creates torrent records,
    /// and adds download tasks. Used by both immediate processing and scheduled jobs.
    /// Errors are logged internally.
    pub async fn process_single(&self, rss: &Rss, global_exclude_filters: &[String]) {
        tracing::debug!("Processing RSS: {}", rss.title);

        // 1. Prepare processing context (get bangumi info)
        let Some(ctx) = self.prepare_context(rss).await else {
            return;
        };

        // 2. Fetch and parse RSS items
        let Some(parsed_items) = self
            .fetch_and_parse_items(rss, global_exclude_filters, &ctx)
            .await
        else {
            return;
        };

        // 3. Build lookup structures for existing torrents
        let Some(lookup) = self.build_torrent_lookup(rss).await else {
            return;
        };

        // 4. Process each item
        for (item, episode, parse_result) in parsed_items {
            self.process_item(rss, &ctx, &lookup, item, episode, &parse_result)
                .await;
        }
    }

    /// Prepare processing context by fetching bangumi info
    async fn prepare_context(&self, rss: &Rss) -> Option<ProcessingContext> {
        let bangumi = match self.bangumi_repo.get_by_id(rss.bangumi_id).await {
            Ok(Some(b)) => b,
            Ok(None) => {
                tracing::error!("[bangumi_id={}] Bangumi not found", rss.bangumi_id);
                return None;
            }
            Err(e) => {
                tracing::error!(
                    "[bangumi_id={}] Failed to get bangumi: {}",
                    rss.bangumi_id,
                    e
                );
                return None;
            }
        };

        Some(ProcessingContext { bangumi })
    }

    /// Fetch RSS feed, apply filters, and parse episode numbers with metadata
    async fn fetch_and_parse_items(
        &self,
        rss: &Rss,
        global_exclude_filters: &[String],
        ctx: &ProcessingContext,
    ) -> Option<Vec<(RssItem, i32, ParseResult)>> {
        // Parse URL to determine source type
        let source = parse_rss_source(&rss.url);

        // Build fetch context from stored cache info
        let fetch_context = FetchContext {
            etag: rss.etag.clone(),
            last_modified: rss.last_modified.clone(),
        };

        // Fetch RSS feed with conditional request (ETag/Last-Modified)
        let fetch_result = match self
            .rss_fetcher
            .fetch_conditional(&source, Some(&fetch_context))
            .await
        {
            Ok(result) => result,
            Err(e) => {
                tracing::error!("[{}] RSS fetch failed: {}", rss.title, e);
                return None;
            }
        };

        // Handle fetch result
        let (items, new_etag, new_last_modified) = match fetch_result {
            FetchResult::NotModified => {
                tracing::debug!("[{}] RSS not modified (HTTP 304), skipping", rss.title);
                return None;
            }
            FetchResult::Modified {
                items,
                etag,
                last_modified,
            } => (items, etag, last_modified),
        };

        // Extract latest pub_date from ALL items BEFORE filtering (for cache update)
        let latest_pub_date_from_feed = items
            .iter()
            .filter_map(|item| item.pub_date.as_ref())
            .max()
            .cloned();

        // Apply pubDate filter (only process items newer than last_pub_date)
        let items = filter_by_pub_date(items, rss.last_pub_date.as_deref());

        // Merge global and RSS-specific exclude filters
        let all_exclude_filters: Vec<String> = global_exclude_filters
            .iter()
            .chain(rss.exclude_filters.iter())
            .cloned()
            .collect();

        // Filter items by include/exclude patterns
        let filtered_items = filter_rss_items(items, &rss.include_filters, &all_exclude_filters);

        // Parse all items upfront to extract episode numbers and metadata
        let mut parsed_items: Vec<_> = filtered_items
            .into_iter()
            .filter_map(|item| {
                let title = item.title();
                match self.parser.parse(title) {
                    Ok(parse_result) => {
                        parse_result
                            .episode
                            .map(|ep| (item, ep, parse_result.clone()))
                    }
                    Err(e) => {
                        tracing::warn!("Failed to parse title '{}': {}", title, e);
                        None
                    }
                }
            })
            .collect();

        // When auto_complete is disabled, only process the latest episode
        if !ctx.bangumi.auto_complete {
            parsed_items.sort_by(|a, b| b.1.cmp(&a.1));
            if let Some((_, ep, _)) = parsed_items.first() {
                tracing::debug!(
                    "auto_complete disabled for bangumi {}: only processing latest episode {}",
                    ctx.bangumi.id,
                    ep
                );
                parsed_items.truncate(1);
            }
        }

        // Pre-filter by priority: keep only highest priority item per episode
        let parsed_items = self.filter_by_priority(parsed_items);

        // Update cache info in database (after successful fetch)
        let latest_pub_date = latest_pub_date_from_feed.or_else(|| rss.last_pub_date.clone());

        if let Err(e) = self
            .rss_repo
            .update_cache(rss.id, new_etag, new_last_modified, latest_pub_date)
            .await
        {
            tracing::warn!("[{}] Failed to update cache info: {}", rss.title, e);
        }

        Some(parsed_items)
    }

    /// Filter parsed items to keep only the highest priority item per episode.
    fn filter_by_priority(
        &self,
        parsed_items: Vec<(RssItem, i32, ParseResult)>,
    ) -> Vec<(RssItem, i32, ParseResult)> {
        if parsed_items.is_empty() {
            return parsed_items;
        }

        // Build priority calculator from current settings
        let settings = self.settings.get();
        let priority_config = settings.priority.to_config();
        let calculator = PriorityCalculator::new(priority_config);

        // Group items by episode number
        let mut episodes_map: HashMap<i32, Vec<(RssItem, i32, ParseResult)>> = HashMap::new();
        for item in parsed_items {
            episodes_map.entry(item.1).or_default().push(item);
        }

        // For each episode, keep only the highest priority item
        let mut result = Vec::new();
        for (episode, items) in episodes_map {
            let item_count = items.len();

            let best_item = items.into_iter().min_by_key(|(_, _, parse_result)| {
                let comparable = ComparableTorrent {
                    subtitle_group: parse_result.subtitle_group.clone(),
                    subtitle_languages: parse_result.subtitle_language.clone(),
                };
                calculator.calculate_score(&comparable)
            });

            if let Some(item) = best_item {
                if item_count > 1 {
                    tracing::debug!(
                        "E{}: kept highest priority item (group={:?}, lang={:?}) from {} candidates",
                        episode,
                        item.2.subtitle_group,
                        item.2.subtitle_language,
                        item_count
                    );
                }
                result.push(item);
            }
        }

        result
    }

    /// Build lookup structures for existing torrents (batch fetch to avoid N+1)
    async fn build_torrent_lookup(&self, rss: &Rss) -> Option<TorrentLookup> {
        let existing_torrents = match self.torrent_repo.get_by_bangumi_id(rss.bangumi_id).await {
            Ok(torrents) => torrents,
            Err(e) => {
                tracing::error!("[{}] Failed to fetch existing torrents: {}", rss.title, e);
                return None;
            }
        };

        Some(TorrentLookup::from_torrents(existing_torrents))
    }

    /// Process a single RSS item: check duplicates, compare priority, create record, add download
    async fn process_item(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        lookup: &TorrentLookup,
        item: RssItem,
        episode: i32,
        parse_result: &ParseResult,
    ) {
        let title = item.title();
        let info_hash = item.info_hash();
        let torrent_url = item.torrent_url();

        // Skip if torrent already exists in database (by info_hash)
        if lookup.existing_hashes.contains(info_hash) {
            tracing::debug!("Skipping existing torrent: {}", title);
            return;
        }

        // Check if episode already exists and handle priority-based washing
        if let Some(existing_torrents) = lookup.episodes_map.get(&episode) {
            if !self.washing.should_wash(existing_torrents, parse_result) {
                tracing::debug!(
                    "[{}] Skipping E{}: existing torrent has higher or equal priority",
                    rss.title,
                    episode
                );
                return;
            }

            // Wash ("洗版"): replace existing torrents with higher priority resource
            let adjusted_episode = ctx.bangumi.adjust_episode(episode);
            let filename = pathgen::generate_filename(
                &ctx.bangumi.title_chinese,
                ctx.bangumi.season,
                adjusted_episode,
                Some(ctx.bangumi.platform.as_str()),
            );

            let params = WashParams {
                bangumi_id: rss.bangumi_id,
                rss_id: Some(rss.id),
                rss_title: &rss.title,
                existing_torrents,
                info_hash,
                torrent_url,
                episode,
                parse_result,
                save_path: &ctx.bangumi.save_path,
                rename: &filename,
            };

            if let Err(e) = self.washing.wash_episode(params).await {
                tracing::error!("[{}] Failed to wash E{}: {}", rss.title, episode, e);
                return;
            }
        } else {
            // No existing torrents, just create and add
            self.create_and_add_task(rss, ctx, info_hash, torrent_url, episode, parse_result)
                .await;
        }
    }

    /// Create torrent record and queue download task
    async fn create_and_add_task(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        info_hash: &str,
        torrent_url: &str,
        episode: i32,
        parse_result: &ParseResult,
    ) {
        let adjusted_episode = ctx.bangumi.adjust_episode(episode);
        let filename = pathgen::generate_filename(
            &ctx.bangumi.title_chinese,
            ctx.bangumi.season,
            adjusted_episode,
            Some(ctx.bangumi.platform.as_str()),
        );

        // 1. Create torrent record in database
        let torrent = CreateTorrent {
            bangumi_id: Some(rss.bangumi_id),
            rss_id: Some(rss.id),
            info_hash: info_hash.to_string(),
            torrent_url: torrent_url.to_string(),
            episode_number: Some(episode),
            subtitle_group: parse_result.subtitle_group.clone(),
            subtitle_languages: parse_result.subtitle_language.clone(),
            resolution: parse_result.resolution.clone(),
        };

        if let Err(e) = self.torrent_repo.create(torrent).await {
            tracing::error!("[{}] Failed to create torrent record: {}", rss.title, e);
            return;
        }

        // 2. Add download task
        if let Err(e) = self
            .downloader
            .add_task(torrent_url, &ctx.bangumi.save_path, &filename)
            .await
        {
            tracing::error!("[{}] Failed to add download task: {}", rss.title, e);
            return;
        }

        tracing::debug!(
            "[{}] Queued download for E{}: {}",
            rss.title,
            episode,
            info_hash
        );
    }

    /// Process multiple RSS subscriptions in batch.
    ///
    /// Groups RSS feeds by bangumi_id to ensure serial processing within same bangumi,
    /// avoiding race conditions when multiple RSS sources target the same bangumi.
    /// Different bangumi groups are processed concurrently for efficiency.
    pub async fn process_batch(&self, rss_list: Vec<Rss>, global_exclude_filters: &[String]) {
        if rss_list.is_empty() {
            return;
        }

        // Group RSS by bangumi_id to avoid concurrent processing of same bangumi
        let mut groups: HashMap<i64, Vec<Rss>> = HashMap::new();
        for rss in rss_list {
            groups.entry(rss.bangumi_id).or_default().push(rss);
        }

        tracing::debug!(
            "Processing {} RSS feeds in {} bangumi groups with concurrency limit {}",
            groups.values().map(|v| v.len()).sum::<usize>(),
            groups.len(),
            RSS_FETCH_CONCURRENCY
        );

        // Process groups concurrently, but RSS within same group serially
        stream::iter(groups.into_values())
            .for_each_concurrent(RSS_FETCH_CONCURRENCY, |group| async {
                for rss in group {
                    self.process_single(&rss, global_exclude_filters).await;
                }
            })
            .await;
    }

    /// Get global exclude filters from settings
    pub fn get_global_exclude_filters(&self) -> Vec<String> {
        let settings = self.settings.get();
        settings.filter.global_rss_filters.clone()
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Parse RSS URL to determine source type
fn parse_rss_source(url: &str) -> RssSource {
    let url_lower = url.to_lowercase();

    if url_lower.contains("nyaa.si") {
        RssSource::Nyaa(url.to_string())
    } else {
        RssSource::Mikan(url.to_string())
    }
}

/// Compile filter strings into regex patterns (case-insensitive)
fn compile_filters(filters: &[String]) -> Vec<Regex> {
    filters
        .iter()
        .filter_map(|pattern| {
            regex::RegexBuilder::new(pattern)
                .case_insensitive(true)
                .build()
                .map_err(|e| {
                    tracing::warn!("Invalid filter regex '{}': {}", pattern, e);
                    e
                })
                .ok()
        })
        .collect()
}

/// Check if title matches any of the filters (OR logic)
fn matches_any_filter(title: &str, filters: &[Regex]) -> bool {
    filters.iter().any(|re| re.is_match(title))
}

/// Filter RSS items by include and exclude filters
fn filter_rss_items(
    items: Vec<RssItem>,
    include_patterns: &[String],
    exclude_patterns: &[String],
) -> Vec<RssItem> {
    let include_filters = compile_filters(include_patterns);
    let exclude_filters = compile_filters(exclude_patterns);

    items
        .into_iter()
        .filter(|item| {
            let title = item.title();
            let include_ok =
                include_filters.is_empty() || matches_any_filter(title, &include_filters);
            let exclude_ok = !matches_any_filter(title, &exclude_filters);

            include_ok && exclude_ok
        })
        .collect()
}

/// Filter RSS items by pubDate, keeping only items newer than last_pub_date
fn filter_by_pub_date(items: Vec<RssItem>, last_pub_date: Option<&str>) -> Vec<RssItem> {
    let Some(last_pub_date) = last_pub_date else {
        return items;
    };

    let before_count = items.len();
    let filtered: Vec<_> = items
        .into_iter()
        .filter(|item| match &item.pub_date {
            Some(pub_date) => pub_date.as_str() > last_pub_date,
            None => true,
        })
        .collect();

    let filtered_count = before_count - filtered.len();
    if filtered_count > 0 {
        tracing::debug!(
            "Filtered {} items older than last_pub_date {}",
            filtered_count,
            last_pub_date
        );
    }

    filtered
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_rss_source_nyaa() {
        let source = parse_rss_source("https://nyaa.si/?page=rss&q=test");
        assert!(matches!(source, RssSource::Nyaa(_)));
    }

    #[test]
    fn test_parse_rss_source_mikan() {
        let source = parse_rss_source("https://mikanani.me/RSS/Bangumi?bangumiId=3376");
        assert!(matches!(source, RssSource::Mikan(_)));
    }

    #[test]
    fn test_filter_by_pub_date_none() {
        let items = vec![
            create_test_item("Item 1", Some("2024-01-02")),
            create_test_item("Item 2", Some("2024-01-01")),
        ];
        let filtered = filter_by_pub_date(items, None);
        assert_eq!(filtered.len(), 2);
    }

    #[test]
    fn test_filter_by_pub_date_some() {
        let items = vec![
            create_test_item("Item 1", Some("2024-01-03")),
            create_test_item("Item 2", Some("2024-01-01")),
        ];
        let filtered = filter_by_pub_date(items, Some("2024-01-02"));
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].title(), "Item 1");
    }

    #[test]
    fn test_filter_rss_items_include() {
        let items = vec![
            create_test_item("ANi Episode 1", None),
            create_test_item("Other Group Episode 1", None),
        ];
        let filtered = filter_rss_items(items, &["ANi".to_string()], &[]);
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].title(), "ANi Episode 1");
    }

    #[test]
    fn test_filter_rss_items_exclude() {
        let items = vec![
            create_test_item("Episode 1 1080p", None),
            create_test_item("Episode 2 720p", None),
        ];
        let filtered = filter_rss_items(items, &[], &["720p".to_string()]);
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].title(), "Episode 1 1080p");
    }

    fn create_test_item(title: &str, pub_date: Option<&str>) -> RssItem {
        RssItem {
            title: title.to_string(),
            torrent_url: "http://example.com/torrent".to_string(),
            info_hash: "abc123".to_string(),
            pub_date: pub_date.map(|s| s.to_string()),
        }
    }
}
