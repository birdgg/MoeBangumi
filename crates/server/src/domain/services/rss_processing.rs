use futures::stream::{self, StreamExt};
use parser::{ParseResult, Parser};
use regex::Regex;
use crate::rss::{FetchContext, FetchResult, RssClient, RssItem, RssSource};
use sqlx::SqlitePool;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Maximum number of RSS feeds to fetch concurrently
const RSS_FETCH_CONCURRENCY: usize = 5;

use crate::models::{BangumiWithMetadata, CreateTorrent, Rss, Torrent};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::washing::{WashParams, WashingService};
use crate::services::{AddTaskOptions, DownloaderHandle, SettingsService};
use washing::{ComparableTorrent, PriorityCalculator};

/// Context for RSS processing, avoiding repeated parameter passing
struct ProcessingContext {
    bangumi: BangumiWithMetadata,
}

/// Lookup structures for existing torrents (O(1) access)
struct TorrentLookup {
    existing_hashes: HashSet<String>,
    episodes_map: HashMap<i32, Vec<Torrent>>,
}

/// Service for processing RSS feeds and creating download tasks.
///
/// This service encapsulates the core RSS processing logic, providing both
/// synchronous and asynchronous interfaces for use by different consumers
/// (BangumiService for immediate processing, RssFetchJob for scheduled processing).
pub struct RssProcessingService {
    db: SqlitePool,
    rss_client: Arc<RssClient>,
    downloader: Arc<DownloaderHandle>,
    settings: Arc<SettingsService>,
    washing: Arc<WashingService>,
    parser: Parser,
}

impl RssProcessingService {
    /// Create a new RSS processing service
    pub fn new(
        db: SqlitePool,
        rss_client: Arc<RssClient>,
        downloader: Arc<DownloaderHandle>,
        settings: Arc<SettingsService>,
        washing: Arc<WashingService>,
    ) -> Self {
        Self {
            db,
            rss_client,
            downloader,
            settings,
            washing,
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
        let bangumi = match BangumiRepository::get_with_metadata_by_id(&self.db, rss.bangumi_id).await {
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
    /// Returns None if:
    /// - RSS feed returned 304 Not Modified (no changes)
    /// - HTTP request failed
    /// - No items after filtering
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
            .rss_client
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
                        // Keep items that have episode numbers, along with full parse result
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
        if !ctx.bangumi.bangumi.auto_complete {
            parsed_items.sort_by(|a, b| b.1.cmp(&a.1));
            if let Some((_, ep, _)) = parsed_items.first() {
                tracing::debug!(
                    "auto_complete disabled for bangumi {}: only processing latest episode {}",
                    ctx.bangumi.bangumi.id,
                    ep
                );
                parsed_items.truncate(1);
            }
        }

        // Pre-filter by priority: keep only highest priority item per episode
        // This prevents multiple items for the same episode from being added
        // when the RSS feed contains multiple subtitle groups or languages
        let parsed_items = self.filter_by_priority(parsed_items);

        // Update cache info in database (after successful fetch)
        // Use latest pub_date from the ORIGINAL feed (before filtering)
        // This ensures we don't re-process old items on next fetch
        let latest_pub_date = latest_pub_date_from_feed.or_else(|| rss.last_pub_date.clone());

        if let Err(e) = RssRepository::update_cache(
            &self.db,
            rss.id,
            new_etag,
            new_last_modified,
            latest_pub_date,
        )
        .await
        {
            tracing::warn!("[{}] Failed to update cache info: {}", rss.title, e);
        }

        Some(parsed_items)
    }

    /// Filter parsed items to keep only the highest priority item per episode.
    ///
    /// This pre-filters items before processing to avoid unnecessary database operations
    /// and download tasks for lower priority resources. Uses the same priority calculation
    /// logic as washing (subtitle group > language combination).
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

            // Find the best item by priority score (lowest score = highest priority)
            let best_item = items
                .into_iter()
                .min_by_key(|(_, _, parse_result)| {
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
        let existing_torrents =
            match TorrentRepository::get_by_bangumi_id(&self.db, rss.bangumi_id).await {
                Ok(torrents) => torrents,
                Err(e) => {
                    tracing::error!("[{}] Failed to fetch existing torrents: {}", rss.title, e);
                    return None;
                }
            };

        let existing_hashes: HashSet<String> = existing_torrents
            .iter()
            .map(|t| t.info_hash.clone())
            .collect();

        let episodes_map: HashMap<i32, Vec<Torrent>> =
            existing_torrents
                .into_iter()
                .fold(HashMap::new(), |mut map, t| {
                    if let Some(ep) = t.episode_number {
                        map.entry(ep).or_default().push(t);
                    }
                    map
                });

        Some(TorrentLookup {
            existing_hashes,
            episodes_map,
        })
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
            let adjusted_episode = ctx.bangumi.metadata.adjust_episode(episode);
            let filename = crate::pathgen::generate_filename(
                &ctx.bangumi.metadata.title_chinese,
                ctx.bangumi.metadata.season,
                adjusted_episode,
                Some(ctx.bangumi.metadata.platform.as_str()),
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
                save_path: &ctx.bangumi.bangumi.save_path,
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
        // Apply episode offset to convert RSS episode number to season-relative episode
        let adjusted_episode = ctx.bangumi.metadata.adjust_episode(episode);

        // Generate filename for this specific episode
        let filename = crate::pathgen::generate_filename(
            &ctx.bangumi.metadata.title_chinese,
            ctx.bangumi.metadata.season,
            adjusted_episode,
            Some(ctx.bangumi.metadata.platform.as_str()),
        );

        // Start transaction
        let mut tx = match self.db.begin().await {
            Ok(tx) => tx,
            Err(e) => {
                tracing::error!("[{}] Failed to start transaction: {}", rss.title, e);
                return;
            }
        };

        // 1. Create torrent record in database
        let torrent = CreateTorrent {
            bangumi_id: rss.bangumi_id,
            rss_id: Some(rss.id),
            info_hash: info_hash.to_string(),
            torrent_url: torrent_url.to_string(),
            episode_number: Some(episode),
            subtitle_group: parse_result.subtitle_group.clone(),
            subtitle_languages: parse_result.subtitle_language.clone(),
            resolution: parse_result.resolution.clone(),
        };

        if let Err(e) = TorrentRepository::create_with_executor(&mut *tx, torrent).await {
            tracing::error!("[{}] Failed to create torrent record: {}", rss.title, e);
            return;
        }

        // 2. Add download task
        let options = AddTaskOptions::new(torrent_url)
            .save_path(&ctx.bangumi.bangumi.save_path)
            .rename(&filename);

        if let Err(e) = self.downloader.add_task(options).await {
            tracing::error!("[{}] Failed to add download task: {}", rss.title, e);
            return;
        }

        // 3. Commit transaction
        if let Err(e) = tx.commit().await {
            tracing::error!("[{}] Failed to commit transaction: {}", rss.title, e);
            return;
        }

        tracing::debug!("[{}] Queued download for E{}: {}", rss.title, episode, info_hash);
    }

    /// Process multiple RSS subscriptions in batch (concurrently with limit).
    ///
    /// Used by the scheduled RSS fetch job to process all enabled subscriptions.
    /// RSS feeds are fetched and processed concurrently for better performance,
    /// with a maximum of `RSS_FETCH_CONCURRENCY` concurrent requests.
    ///
    /// With the priority-based washing system, all RSS feeds are processed equally.
    /// Higher priority resources will automatically replace lower priority ones
    /// based on subtitle group, language, and resolution settings.
    pub async fn process_batch(&self, rss_list: Vec<Rss>, global_exclude_filters: &[String]) {
        if rss_list.is_empty() {
            return;
        }

        tracing::debug!(
            "Processing {} RSS feeds with concurrency limit {}",
            rss_list.len(),
            RSS_FETCH_CONCURRENCY
        );

        stream::iter(rss_list)
            .map(|rss| async move { self.process_single(&rss, global_exclude_filters).await })
            .buffer_unordered(RSS_FETCH_CONCURRENCY)
            .collect::<Vec<_>>()
            .await;
    }

    /// Spawn background tasks to process RSS subscriptions by their IDs.
    ///
    /// This method is used by BangumiService to trigger immediate RSS processing
    /// after creating or updating a bangumi with new RSS subscriptions.
    /// The processing happens asynchronously and does not block the API response.
    pub fn spawn_background(&self, rss_ids: Vec<i64>) {
        if rss_ids.is_empty() {
            return;
        }

        let db = self.db.clone();
        let rss_client = Arc::clone(&self.rss_client);
        let downloader = Arc::clone(&self.downloader);
        let settings = Arc::clone(&self.settings);
        let washing = Arc::clone(&self.washing);

        tokio::spawn(async move {
            tracing::info!(
                "Starting background RSS fetch for {} subscriptions",
                rss_ids.len()
            );

            // Create a temporary service instance for background processing
            let service =
                RssProcessingService::new(db.clone(), rss_client, downloader, settings.clone(), washing);

            // Get global exclude filters from settings
            let settings_data = settings.get();
            let global_exclude_filters = &settings_data.filter.global_rss_filters;

            for rss_id in rss_ids {
                // Fetch RSS from database
                let rss = match RssRepository::get_by_id(&db, rss_id).await {
                    Ok(Some(rss)) => rss,
                    Ok(None) => {
                        tracing::warn!("RSS {} not found, skipping", rss_id);
                        continue;
                    }
                    Err(e) => {
                        tracing::error!("Failed to fetch RSS {}: {}", rss_id, e);
                        continue;
                    }
                };

                service.process_single(&rss, global_exclude_filters).await;
            }

            tracing::info!("Background RSS fetch completed");
        });
    }

    /// Get global exclude filters from settings
    pub fn get_global_exclude_filters(&self) -> Vec<String> {
        let settings = self.settings.get();
        settings.filter.global_rss_filters.clone()
    }
}

/// Parse RSS URL to determine source type
fn parse_rss_source(url: &str) -> RssSource {
    let url_lower = url.to_lowercase();

    if url_lower.contains("nyaa.si") {
        RssSource::Nyaa(url.to_string())
    } else {
        // Default to Mikan (covers mikanani.me and other sources)
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
///
/// Filtering logic (both conditions must be satisfied):
/// 1. Include filter: If include_patterns is empty, all items pass;
///    otherwise item must match at least one include pattern (OR logic)
/// 2. Exclude filter: Item must NOT match any exclude pattern
///
/// Returns items that satisfy BOTH conditions (AND logic)
fn filter_rss_items(
    items: Vec<crate::rss::RssItem>,
    include_patterns: &[String],
    exclude_patterns: &[String],
) -> Vec<crate::rss::RssItem> {
    let include_filters = compile_filters(include_patterns);
    let exclude_filters = compile_filters(exclude_patterns);

    items
        .into_iter()
        .filter(|item| {
            let title = item.title();
            // Must match ANY include filter (if include is not empty)
            let include_ok =
                include_filters.is_empty() || matches_any_filter(title, &include_filters);
            // Must NOT match ANY exclude filter
            let exclude_ok = !matches_any_filter(title, &exclude_filters);

            include_ok && exclude_ok
        })
        .collect()
}

/// Filter RSS items by pubDate, keeping only items newer than last_pub_date
/// Items without pub_date are always included (conservative approach)
fn filter_by_pub_date(items: Vec<RssItem>, last_pub_date: Option<&str>) -> Vec<RssItem> {
    let Some(last_pub_date) = last_pub_date else {
        // No previous pub_date stored, include all items
        return items;
    };

    let before_count = items.len();
    let filtered: Vec<_> = items
        .into_iter()
        .filter(|item| {
            match &item.pub_date {
                Some(pub_date) => pub_date.as_str() > last_pub_date,
                // Items without pub_date are included (conservative)
                None => true,
            }
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
