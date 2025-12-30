use downloader::AddTaskOptions;
use parser::Parser;
use regex::Regex;
use rss::{RssClient, RssItem, RssSource};
use sqlx::SqlitePool;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::models::{Bangumi, CreateTorrent, Rss, Torrent};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::{DownloaderService, SettingsService};

/// Context for RSS processing, avoiding repeated parameter passing
struct ProcessingContext {
    bangumi: Bangumi,
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
    downloader: Arc<DownloaderService>,
    settings: Arc<SettingsService>,
    parser: Parser,
}

impl RssProcessingService {
    /// Create a new RSS processing service
    pub fn new(
        db: SqlitePool,
        rss_client: Arc<RssClient>,
        downloader: Arc<DownloaderService>,
        settings: Arc<SettingsService>,
    ) -> Self {
        Self {
            db,
            rss_client,
            downloader,
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
        for (item, episode) in parsed_items {
            self.process_item(rss, &ctx, &lookup, item, episode).await;
        }
    }

    /// Prepare processing context by fetching bangumi info
    async fn prepare_context(&self, rss: &Rss) -> Option<ProcessingContext> {
        let bangumi = match BangumiRepository::get_by_id(&self.db, rss.bangumi_id).await {
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

    /// Fetch RSS feed, apply filters, and parse episode numbers
    async fn fetch_and_parse_items(
        &self,
        rss: &Rss,
        global_exclude_filters: &[String],
        ctx: &ProcessingContext,
    ) -> Option<Vec<(RssItem, i32)>> {
        // Parse URL to determine source type
        let source = parse_rss_source(&rss.url);

        // Fetch RSS feed
        let items = match self.rss_client.fetch(&source).await {
            Ok(items) => items,
            Err(e) => {
                tracing::error!("[{}] RSS fetch failed: {}", rss.title, e);
                return None;
            }
        };

        // Merge global and RSS-specific exclude filters
        let all_exclude_filters: Vec<String> = global_exclude_filters
            .iter()
            .chain(rss.exclude_filters.iter())
            .cloned()
            .collect();

        // Filter items by include/exclude patterns
        let filtered_items = filter_rss_items(items, &rss.include_filters, &all_exclude_filters);

        // Parse all items upfront to extract episode numbers
        let mut parsed_items: Vec<_> = filtered_items
            .into_iter()
            .filter_map(|item| {
                let title = item.title();
                match self.parser.parse(title) {
                    Ok(parse_result) => parse_result.episode.map(|ep| (item, ep)),
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
            if let Some((_, ep)) = parsed_items.first() {
                tracing::debug!(
                    "auto_complete disabled for bangumi {}: only processing latest episode {}",
                    ctx.bangumi.id,
                    ep
                );
                parsed_items.truncate(1);
            }
        }

        Some(parsed_items)
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

    /// Process a single RSS item: check duplicates, handle override, create record, add download
    async fn process_item(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        lookup: &TorrentLookup,
        item: RssItem,
        episode: i32,
    ) {
        let title = item.title();
        let info_hash = item.info_hash();
        let torrent_url = item.torrent_url();

        // Skip if torrent already exists in database (by info_hash)
        if lookup.existing_hashes.contains(info_hash) {
            tracing::debug!("Skipping existing torrent: {}", title);
            return;
        }

        // Check if episode already exists and handle override logic
        if let Some(existing_torrents) = lookup.episodes_map.get(&episode) {
            if !self
                .should_override_existing(rss, existing_torrents, episode)
                .await
            {
                return;
            }

            // Override ("洗版"): replace existing torrents from backup RSS
            self.wash_episode(rss, ctx, existing_torrents, info_hash, torrent_url, episode)
                .await;
        } else {
            // No existing torrents, just create and add
            self.create_and_add_task(rss, ctx, info_hash, torrent_url, episode)
                .await;
        }
    }

    /// Wash episode: atomically delete old torrents and create new one in a transaction
    async fn wash_episode(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        existing_torrents: &[Torrent],
        info_hash: &str,
        torrent_url: &str,
        episode: i32,
    ) {
        if existing_torrents.len() > 1 {
            tracing::warn!(
                "[{}] Found {} torrents for E{}, expected 1. Will delete all.",
                rss.title,
                existing_torrents.len(),
                episode
            );
        }

        tracing::info!(
            "[{}] Washing E{}: replacing backup RSS with primary RSS",
            rss.title,
            episode
        );

        // Collect info_hashes for downloader cleanup (done after transaction)
        let old_hashes: Vec<String> = existing_torrents
            .iter()
            .map(|t| t.info_hash.clone())
            .collect();

        // Use transaction to ensure atomicity: delete old + create new
        let mut tx = match self.db.begin().await {
            Ok(tx) => tx,
            Err(e) => {
                tracing::error!("[{}] Failed to begin transaction: {}", rss.title, e);
                return;
            }
        };

        // Delete all existing torrents in transaction
        for existing in existing_torrents {
            if let Err(e) = TorrentRepository::delete_with_executor(&mut *tx, existing.id).await {
                tracing::error!(
                    "[{}] Failed to delete backup torrent from database: {}",
                    rss.title,
                    e
                );
                // Transaction will be rolled back on drop
                return;
            }
        }

        // Create new torrent in transaction
        let new_torrent = CreateTorrent {
            bangumi_id: rss.bangumi_id,
            rss_id: Some(rss.id),
            info_hash: info_hash.to_string(),
            torrent_url: torrent_url.to_string(),
            episode_number: Some(episode),
        };

        if let Err(e) = TorrentRepository::create_with_executor(&mut *tx, new_torrent).await {
            tracing::error!("[{}] Failed to create new torrent: {}", rss.title, e);
            return;
        }

        // Commit transaction
        if let Err(e) = tx.commit().await {
            tracing::error!("[{}] Failed to commit transaction: {}", rss.title, e);
            return;
        }

        // Transaction committed successfully, now handle downloader operations (best-effort)
        // Delete old torrents from downloader
        for hash in &old_hashes {
            self.delete_from_downloader(hash, rss).await;
        }

        // Add new download task
        self.add_download_task(rss, ctx, info_hash, torrent_url, episode)
            .await;
    }

    /// Determine if we should override existing episode torrents
    ///
    /// Returns true only if:
    /// - Current RSS is primary, AND
    /// - ALL existing torrents are from backup RSS (not primary, not manual)
    async fn should_override_existing(
        &self,
        rss: &Rss,
        existing_torrents: &[Torrent],
        episode: i32,
    ) -> bool {
        // Backup RSS should not override existing episodes
        if !rss.is_primary {
            tracing::debug!("[{}] Skipping already downloaded episode E{}", rss.title, episode);
            return false;
        }

        // Check for manual adds first (no RSS ID means manual add, don't override)
        for existing in existing_torrents {
            if existing.rss_id.is_none() {
                tracing::debug!(
                    "[{}] Skipping E{}: torrent {} is manually added",
                    rss.title,
                    episode,
                    existing.id
                );
                return false;
            }
        }

        // Batch fetch all RSS entities for existing torrents
        let rss_ids: Vec<i64> = existing_torrents
            .iter()
            .filter_map(|t| t.rss_id)
            .collect();

        let existing_rss_list = match RssRepository::get_by_ids(&self.db, &rss_ids).await {
            Ok(list) => list,
            Err(e) => {
                tracing::error!("[{}] Failed to fetch existing RSS: {}", rss.title, e);
                return false;
            }
        };

        // Build a map for quick lookup
        let rss_map: std::collections::HashMap<i64, &Rss> = existing_rss_list
            .iter()
            .map(|r| (r.id, r))
            .collect();

        // Check if ALL existing torrents are from backup RSS
        for existing in existing_torrents {
            if let Some(existing_rss_id) = existing.rss_id {
                let can_override = match rss_map.get(&existing_rss_id) {
                    Some(existing_rss) => !existing_rss.is_primary,
                    None => true, // RSS deleted, allow override
                };

                if !can_override {
                    tracing::debug!(
                        "[{}] Skipping E{}: torrent {} is from primary RSS",
                        rss.title,
                        episode,
                        existing.id
                    );
                    return false;
                }
            }
        }

        true
    }

    /// Create torrent record in database and add download task to downloader
    async fn create_and_add_task(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        info_hash: &str,
        torrent_url: &str,
        episode: i32,
    ) {
        // Create torrent record with parsed episode number
        if let Err(e) = TorrentRepository::create(
            &self.db,
            CreateTorrent {
                bangumi_id: rss.bangumi_id,
                rss_id: Some(rss.id),
                info_hash: info_hash.to_string(),
                torrent_url: torrent_url.to_string(),
                episode_number: Some(episode),
            },
        )
        .await
        {
            tracing::error!("[{}] Database error: {}", rss.title, e);
            return;
        }

        self.add_download_task(rss, ctx, info_hash, torrent_url, episode)
            .await;
    }

    /// Add download task to downloader (without creating database record)
    async fn add_download_task(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        info_hash: &str,
        torrent_url: &str,
        episode: i32,
    ) {
        // Generate filename for this specific episode
        let filename = pathgen::generate_filename(
            &ctx.bangumi.title_chinese,
            ctx.bangumi.season,
            episode,
            Some(ctx.bangumi.platform.as_str()),
        );

        // Add to downloader with "moe" tag to identify moe-managed tasks
        // Add "rename" tag so RenameService will process it after download completes
        let options = AddTaskOptions::new(torrent_url)
            .save_path(&ctx.bangumi.save_path)
            .rename(&filename)
            .add_tag("moe")
            .add_tag("rename");

        match self.downloader.add_task(options).await {
            Ok(_) => {
                tracing::debug!("Added to downloader: {}", info_hash);
            }
            Err(e) => {
                tracing::error!("[{}] Failed to add download task: {}", rss.title, e);
            }
        }
    }

    /// Delete a torrent from the downloader by info_hash.
    ///
    /// Used during "washing" (洗版) when a primary RSS torrent replaces
    /// a backup RSS torrent. The old torrent task and its downloaded files
    /// are removed from the downloader.
    ///
    /// Errors are logged but not propagated as critical failures since:
    /// - Task might not exist (already deleted/completed manually)
    /// - Downloader connection issues shouldn't block database cleanup
    async fn delete_from_downloader(&self, info_hash: &str, rss: &Rss) {
        match self.downloader.delete_task(&[info_hash], true).await {
            Ok(_) => {
                tracing::info!("[{}] Deleted old torrent from downloader: {}", rss.title, info_hash);
            }
            Err(e) => {
                tracing::warn!(
                    "[{}] Could not delete torrent from downloader (hash={}): {}",
                    rss.title,
                    info_hash,
                    e
                );
            }
        }
    }

    /// Process multiple RSS subscriptions in batch (concurrently).
    ///
    /// Used by the scheduled RSS fetch job to process all enabled subscriptions.
    /// RSS feeds are fetched and processed concurrently for better performance.
    ///
    /// Processing is done in two phases to ensure primary RSS takes priority:
    /// 1. First, all backup RSS feeds are processed concurrently
    /// 2. Then, all primary RSS feeds are processed concurrently
    ///
    /// Primary RSS can override episodes downloaded by backup RSS, ensuring
    /// the preferred source is always used when available.
    pub async fn process_batch(&self, rss_list: Vec<Rss>, global_exclude_filters: &[String]) {
        // Partition into primary and backup RSS
        let (primary, backup): (Vec<_>, Vec<_>) =
            rss_list.into_iter().partition(|rss| rss.is_primary);

        // Phase 1: Process all backup RSS concurrently
        // Backup RSS downloads first as a fallback
        if !backup.is_empty() {
            tracing::debug!("Processing {} backup RSS feeds", backup.len());
            let futures: Vec<_> = backup
                .iter()
                .map(|rss| self.process_single(rss, global_exclude_filters))
                .collect();
            futures::future::join_all(futures).await;
        }

        // Phase 2: Process all primary RSS concurrently
        // Primary RSS can override episodes from backup RSS
        if !primary.is_empty() {
            tracing::debug!("Processing {} primary RSS feeds", primary.len());
            let futures: Vec<_> = primary
                .iter()
                .map(|rss| self.process_single(rss, global_exclude_filters))
                .collect();
            futures::future::join_all(futures).await;
        }
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

        tokio::spawn(async move {
            tracing::info!(
                "Starting background RSS fetch for {} subscriptions",
                rss_ids.len()
            );

            // Create a temporary service instance for background processing
            let service =
                RssProcessingService::new(db.clone(), rss_client, downloader, settings.clone());

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

/// Check if title matches all of the include filters (AND logic)
fn matches_all_filters(title: &str, filters: &[Regex]) -> bool {
    filters.iter().all(|re| re.is_match(title))
}

/// Filter RSS items by include/exclude filters
fn filter_rss_items(
    items: Vec<rss::RssItem>,
    include_patterns: &[String],
    exclude_patterns: &[String],
) -> Vec<rss::RssItem> {
    let include_filters = compile_filters(include_patterns);
    let exclude_filters = compile_filters(exclude_patterns);

    items
        .into_iter()
        .filter(|item| {
            let title = item.title();

            // Must match ALL include filters (if any)
            if !include_filters.is_empty() && !matches_all_filters(title, &include_filters) {
                return false;
            }

            // Must NOT match ANY exclude filter
            !matches_any_filter(title, &exclude_filters)
        })
        .collect()
}
