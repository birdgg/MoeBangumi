use downloader::AddTaskOptions;
use parser::Parser;
use regex::Regex;
use rss::{RssClient, RssSource};
use sqlx::SqlitePool;
use std::sync::Arc;

use crate::models::{CreateTorrent, Rss};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::{DownloaderService, SettingsService};

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
        tracing::debug!("Processing RSS: {} (id={})", rss.url, rss.id);

        // Get bangumi info for path generation and error context
        let bangumi = match BangumiRepository::get_by_id(&self.db, rss.bangumi_id).await {
            Ok(Some(b)) => b,
            Ok(None) => {
                tracing::error!("[bangumi_id={}] Bangumi not found", rss.bangumi_id);
                return;
            }
            Err(e) => {
                tracing::error!(
                    "[bangumi_id={}] Failed to get bangumi: {}",
                    rss.bangumi_id,
                    e
                );
                return;
            }
        };

        // Error context with bangumi name and season
        let ctx = format!("[{} S{}]", bangumi.title_chinese, bangumi.season);

        // Parse URL to determine source type
        let source = parse_rss_source(&rss.url);

        // Fetch RSS feed
        let items = match self.rss_client.fetch(&source).await {
            Ok(items) => items,
            Err(e) => {
                tracing::error!("{} RSS fetch failed: {}", ctx, e);
                return;
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
        // This avoids parsing twice when auto_complete is disabled
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
        if !bangumi.auto_complete {
            // Sort by episode number descending and keep only the latest
            parsed_items.sort_by(|a, b| b.1.cmp(&a.1));
            if let Some((_, ep)) = parsed_items.first() {
                tracing::debug!(
                    "auto_complete disabled for bangumi {}: only processing latest episode {}",
                    bangumi.id,
                    ep
                );
                parsed_items.truncate(1);
            }
        }

        for (item, episode) in parsed_items {
            let title = item.title();
            let info_hash = item.info_hash();
            let torrent_url = item.torrent_url();

            // Skip if torrent already exists in database (by info_hash)
            match TorrentRepository::exists_by_info_hash(&self.db, info_hash).await {
                Ok(true) => {
                    tracing::debug!("Skipping existing torrent: {}", title);
                    continue;
                }
                Ok(false) => {}
                Err(e) => {
                    tracing::error!("{} Failed to check torrent existence: {}", ctx, e);
                    continue;
                }
            }

            // Check if episode already exists
            // Primary RSS can override episodes from backup RSS
            let existing_torrents =
                match TorrentRepository::get_by_bangumi_episode(&self.db, rss.bangumi_id, episode)
                    .await
                {
                    Ok(torrents) => torrents,
                    Err(e) => {
                        tracing::error!("{} Failed to check episode existence: {}", ctx, e);
                        continue;
                    }
                };

            if !existing_torrents.is_empty() {
                // Episode already exists, check if we should override
                if !rss.is_primary {
                    // Backup RSS should not override existing episodes
                    tracing::debug!(
                        "Skipping already downloaded episode: {} E{}",
                        bangumi.title_chinese,
                        episode
                    );
                    continue;
                }

                // Primary RSS: check if existing torrent is from a non-primary RSS
                let existing = &existing_torrents[0];
                let should_override = if let Some(existing_rss_id) = existing.rss_id {
                    // Check if existing torrent's RSS is primary
                    match RssRepository::get_by_id(&self.db, existing_rss_id).await {
                        Ok(Some(existing_rss)) => !existing_rss.is_primary,
                        Ok(None) => true, // RSS deleted, allow override
                        Err(e) => {
                            tracing::error!("{} Failed to check existing RSS: {}", ctx, e);
                            continue;
                        }
                    }
                } else {
                    // No RSS ID (manual add), don't override
                    false
                };

                if !should_override {
                    tracing::debug!(
                        "Skipping episode already from primary RSS: {} E{}",
                        bangumi.title_chinese,
                        episode
                    );
                    continue;
                }

                // Override: delete existing torrent from backup RSS
                tracing::info!(
                    "{} Overriding backup RSS torrent with primary RSS for E{}",
                    ctx,
                    episode
                );
                if let Err(e) = TorrentRepository::delete(&self.db, existing.id).await {
                    tracing::error!("{} Failed to delete backup torrent: {}", ctx, e);
                    continue;
                }
            }

            // Create torrent record with parsed episode number
            if let Err(e) = TorrentRepository::create(
                &self.db,
                CreateTorrent {
                    bangumi_id: rss.bangumi_id,
                    rss_id: Some(rss.id),
                    info_hash: info_hash.to_string(),
                    torrent_url: torrent_url.to_string(),
                    kind: Default::default(), // Episode
                    episode_number: Some(episode),
                },
            )
            .await
            {
                tracing::error!("{} Database error: {}", ctx, e);
                continue;
            }

            // Use the save_path stored in bangumi (auto-generated when bangumi was created)
            // and generate filename for this specific episode
            let save_path = bangumi.save_path.clone();

            let filename = pathgen::generate_filename(
                &bangumi.title_chinese,
                bangumi.season,
                episode,
                Some(bangumi.platform.as_str()),
            );

            // Add to downloader with "moe" tag to identify moe-managed tasks
            let options = AddTaskOptions::new(torrent_url)
                .save_path(&save_path)
                .rename(&filename)
                .add_tag("moe");

            match self.downloader.add_task(options).await {
                Ok(_) => {
                    tracing::debug!("Added to downloader: {}", title);
                }
                Err(e) => {
                    tracing::error!("{} Failed to add download task: {}", ctx, e);
                }
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
