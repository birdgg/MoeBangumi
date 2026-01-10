//! RSS processing service implementation.

use futures::stream::{self, StreamExt};
use parser::ParseResult;
use rss::{FetchContext, FetchResult, RssClient, RssItem};
use sqlx::SqlitePool;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use washing::ComparableTorrent;

use crate::models::{BangumiWithSeries, CreateTorrent, Rss, Torrent};
use crate::repositories::RssRepository;
use crate::services::washing::WashingService;
use crate::services::{AddTaskOptions, DownloaderHandle, SettingsService};

use super::adapters::{DefaultRssFetcher, DefaultTaskScheduler, SqliteRssDataAccess};
use super::filters::{filter_by_pub_date, filter_rss_items, parse_rss_source, MAX_ITEMS_PER_RSS};
use super::processor::RssProcessor;
use super::traits::{ProcessAction, RssDataAccess, RssFetcher, TaskScheduler};

/// Maximum number of RSS feeds to fetch concurrently
const RSS_FETCH_CONCURRENCY: usize = 5;

/// Context for RSS processing
struct ProcessingContext {
    bangumi: BangumiWithSeries,
    save_path: String,
}

/// Lookup structures for existing torrents
struct TorrentLookup {
    existing_hashes: HashSet<String>,
    episode_torrents: HashMap<i32, Vec<(Torrent, ComparableTorrent)>>,
}

/// Service for processing RSS feeds and creating download tasks.
pub struct RssProcessingService {
    data_access: Arc<dyn RssDataAccess>,
    fetcher: Arc<dyn RssFetcher>,
    scheduler: Arc<dyn TaskScheduler>,
    settings: Arc<SettingsService>,
    processor: RssProcessor,
    // Keep these for spawn_background
    db: SqlitePool,
    rss_client: Arc<RssClient>,
    downloader: Arc<DownloaderHandle>,
    washing: Arc<WashingService>,
}

impl RssProcessingService {
    /// Create a new RSS processing service with default implementations
    pub fn new(
        db: SqlitePool,
        rss_client: Arc<RssClient>,
        downloader: Arc<DownloaderHandle>,
        settings: Arc<SettingsService>,
        washing: Arc<WashingService>,
    ) -> Self {
        let data_access = Arc::new(SqliteRssDataAccess::new(db.clone()));
        let fetcher = Arc::new(DefaultRssFetcher::new(Arc::clone(&rss_client)));
        let scheduler = Arc::new(DefaultTaskScheduler::new(Arc::clone(&downloader)));

        Self {
            data_access,
            fetcher,
            scheduler,
            settings,
            processor: RssProcessor::new(),
            db,
            rss_client,
            downloader,
            washing,
        }
    }

    /// Process a single RSS subscription
    pub async fn process_single(&self, rss: &Rss, global_exclude_filters: &[String]) {
        tracing::debug!("Processing RSS: {}", rss.title);

        let Some(ctx) = self.prepare_context(rss).await else {
            return;
        };

        let Some(parsed_items) = self
            .fetch_and_parse_items(rss, global_exclude_filters, &ctx)
            .await
        else {
            return;
        };

        let Some(lookup) = self.build_torrent_lookup(rss).await else {
            return;
        };

        for (item, episode, parse_result) in parsed_items {
            self.process_item(rss, &ctx, &lookup, item, episode, &parse_result)
                .await;
        }
    }

    async fn prepare_context(&self, rss: &Rss) -> Option<ProcessingContext> {
        let bangumi = match self.data_access.get_bangumi_with_series(rss.bangumi_id).await {
            Ok(Some(b)) => b,
            Ok(None) => {
                tracing::error!("[bangumi_id={}] Bangumi not found", rss.bangumi_id);
                return None;
            }
            Err(e) => {
                tracing::error!("[bangumi_id={}] Failed to get bangumi: {}", rss.bangumi_id, e);
                return None;
            }
        };

        let settings = self.settings.get();
        let base_path = &settings.downloader.save_path;

        let save_path = match pathgen::generate_directory(
            base_path,
            &bangumi.series.title_chinese,
            bangumi.bangumi.year,
            bangumi.bangumi.season,
            bangumi.series.tmdb_id,
            Some(bangumi.bangumi.platform.as_str()),
        ) {
            Ok(path) => path,
            Err(e) => {
                tracing::error!("[bangumi_id={}] Failed to generate save path: {}", rss.bangumi_id, e);
                return None;
            }
        };

        Some(ProcessingContext { bangumi, save_path })
    }

    async fn fetch_and_parse_items(
        &self,
        rss: &Rss,
        global_exclude_filters: &[String],
        ctx: &ProcessingContext,
    ) -> Option<Vec<(RssItem, i32, ParseResult)>> {
        let source = parse_rss_source(&rss.url);
        let fetch_context = FetchContext {
            etag: rss.etag.clone(),
            last_modified: rss.last_modified.clone(),
        };

        let fetch_result = match self.fetcher.fetch_conditional(&source, Some(&fetch_context)).await {
            Ok(result) => result,
            Err(e) => {
                tracing::error!("[{}] RSS fetch failed: {}", rss.title, e);
                return None;
            }
        };

        let (items, new_etag, new_last_modified) = match fetch_result {
            FetchResult::NotModified => {
                tracing::debug!("[{}] RSS not modified (HTTP 304), skipping", rss.title);
                return None;
            }
            FetchResult::Modified { items, etag, last_modified } => (items, etag, last_modified),
        };

        let latest_pub_date_from_feed = items
            .iter()
            .filter_map(|item| item.pub_date.as_ref())
            .max()
            .cloned();

        let items = filter_by_pub_date(items, rss.last_pub_date.as_deref());

        let all_exclude_filters: Vec<String> = global_exclude_filters
            .iter()
            .chain(rss.exclude_filters.iter())
            .cloned()
            .collect();

        let mut filtered_items = filter_rss_items(items, &rss.include_filters, &all_exclude_filters);

        if filtered_items.len() > MAX_ITEMS_PER_RSS {
            tracing::warn!(
                "[{}] RSS has {} items, limiting to {}",
                rss.title,
                filtered_items.len(),
                MAX_ITEMS_PER_RSS
            );
            filtered_items.truncate(MAX_ITEMS_PER_RSS);
        }

        let mut parsed_items = self.processor.parse_items(filtered_items);

        parsed_items = self.processor.limit_to_latest_episode(
            parsed_items,
            ctx.bangumi.bangumi.auto_complete,
            ctx.bangumi.bangumi.id,
        );

        let settings = self.settings.get();
        let priority_config = settings.priority.to_config();
        let parsed_items = self.processor.filter_by_priority(parsed_items, priority_config);

        let latest_pub_date = latest_pub_date_from_feed.or_else(|| rss.last_pub_date.clone());

        if let Err(e) = self
            .data_access
            .update_rss_cache(rss.id, new_etag, new_last_modified, latest_pub_date)
            .await
        {
            tracing::warn!("[{}] Failed to update cache info: {}", rss.title, e);
        }

        Some(parsed_items)
    }

    async fn build_torrent_lookup(&self, rss: &Rss) -> Option<TorrentLookup> {
        let existing_torrents = match self.data_access.get_bangumi_torrents(rss.bangumi_id).await {
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

        let mut episode_torrents: HashMap<i32, Vec<(Torrent, ComparableTorrent)>> = HashMap::new();
        for torrent in existing_torrents {
            if let Some((episode, metadata)) = self.get_torrent_episode_and_metadata(&torrent).await {
                episode_torrents.entry(episode).or_default().push((torrent, metadata));
            }
        }

        Some(TorrentLookup { existing_hashes, episode_torrents })
    }

    async fn get_torrent_episode_and_metadata(&self, torrent: &Torrent) -> Option<(i32, ComparableTorrent)> {
        let files = self.scheduler.get_task_files(&torrent.info_hash).await.ok()?;
        let video_file = files.iter().find(|f| f.is_video())?;
        let result = self.processor.parse_filename(&video_file.path)?;

        let metadata = ComparableTorrent {
            subtitle_group: result.subtitle_group,
            subtitle_languages: result.subtitle_language,
        };

        Some((result.episode?, metadata))
    }

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
        let settings = self.settings.get();
        let priority_config = settings.priority.to_config();

        let action = self.processor.determine_action(
            &item,
            episode,
            parse_result,
            &lookup.existing_hashes,
            &lookup.episode_torrents,
            &priority_config,
            &ctx.bangumi,
            &ctx.save_path,
            Some(rss.id),
            &rss.title,
        );

        match action {
            ProcessAction::Skip { reason } => {
                tracing::debug!("Skipping torrent '{}': {}", title, reason);
            }
            ProcessAction::Add {
                info_hash, torrent_url, episode, filename, save_path, bangumi_id, rss_id, ..
            } => {
                self.execute_add(&rss.title, &info_hash, &torrent_url, episode, &filename, &save_path, bangumi_id, rss_id).await;
            }
            ProcessAction::Wash {
                info_hash, torrent_url, episode, adjusted_episode, filename, save_path,
                bangumi_id, rss_id, existing_torrent_ids, existing_info_hashes,
            } => {
                self.execute_wash(
                    &rss.title, &info_hash, &torrent_url, episode, adjusted_episode, &filename,
                    &save_path, bangumi_id, rss_id, &existing_torrent_ids, &existing_info_hashes, parse_result,
                ).await;
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    async fn execute_add(
        &self,
        rss_title: &str,
        info_hash: &str,
        torrent_url: &str,
        episode: i32,
        filename: &str,
        save_path: &str,
        bangumi_id: i64,
        rss_id: Option<i64>,
    ) {
        let torrent = CreateTorrent {
            rss_id,
            info_hash: info_hash.to_string(),
            torrent_url: torrent_url.to_string(),
            bangumi_ids: vec![bangumi_id],
        };

        if let Err(e) = self.data_access.create_torrent(torrent).await {
            tracing::error!("[{}] Failed to create torrent record: {}", rss_title, e);
            return;
        }

        let options = AddTaskOptions::new(torrent_url).save_path(save_path).rename(filename);

        if let Err(e) = self.scheduler.add_download_task(options).await {
            tracing::error!("[{}] Failed to add download task: {}", rss_title, e);
            return;
        }

        tracing::debug!("[{}] Queued download for E{}: {}", rss_title, episode, info_hash);
    }

    #[allow(clippy::too_many_arguments)]
    async fn execute_wash(
        &self,
        rss_title: &str,
        info_hash: &str,
        torrent_url: &str,
        episode: i32,
        _adjusted_episode: i32,
        filename: &str,
        save_path: &str,
        bangumi_id: i64,
        rss_id: Option<i64>,
        existing_torrent_ids: &[i64],
        existing_info_hashes: &[String],
        parse_result: &ParseResult,
    ) {
        tracing::info!(
            "[{}] Washing E{}: replacing {} existing torrent(s) (group={:?}, lang={:?})",
            rss_title, episode, existing_torrent_ids.len(),
            parse_result.subtitle_group, parse_result.subtitle_language,
        );

        if let Err(e) = self.data_access.delete_torrents(existing_torrent_ids).await {
            tracing::error!("[{}] Failed to delete old torrent records: {}", rss_title, e);
            return;
        }

        let torrent = CreateTorrent {
            rss_id,
            info_hash: info_hash.to_string(),
            torrent_url: torrent_url.to_string(),
            bangumi_ids: vec![bangumi_id],
        };

        if let Err(e) = self.data_access.create_torrent(torrent).await {
            tracing::error!("[{}] Failed to create new torrent record: {}", rss_title, e);
            return;
        }

        let hash_refs: Vec<&str> = existing_info_hashes.iter().map(|s| s.as_str()).collect();
        if let Err(e) = self.scheduler.delete_tasks(&hash_refs, true).await {
            tracing::error!("[{}] Failed to delete old tasks from downloader: {}", rss_title, e);
        }

        let options = AddTaskOptions::new(torrent_url).save_path(save_path).rename(filename);

        if let Err(e) = self.scheduler.add_download_task(options).await {
            tracing::error!("[{}] Failed to add new download task: {}", rss_title, e);
            return;
        }

        tracing::info!("[{}] Washed E{}: replaced {} torrent(s)", rss_title, episode, existing_torrent_ids.len());
    }

    /// Process multiple RSS subscriptions in batch
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

    /// Spawn background tasks to process RSS subscriptions by their IDs
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
            tracing::info!("Starting background RSS fetch for {} subscriptions", rss_ids.len());

            let service = RssProcessingService::new(db.clone(), rss_client, downloader, settings.clone(), washing);
            let settings_data = settings.get();
            let global_exclude_filters = &settings_data.filter.global_rss_filters;

            for rss_id in rss_ids {
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
