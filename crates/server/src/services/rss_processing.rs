use downloader::AddTorrentOptions;
use parser::Parser;
use regex::Regex;
use rss::{RssClient, RssSource};
use sqlx::SqlitePool;
use std::sync::Arc;
use thiserror::Error;

use crate::models::{CreateDownloadTask, CreateTorrent, DownloadTaskStatus, Rss};
use crate::repositories::{
    BangumiRepository, DownloadTaskRepository, RssRepository, TorrentRepository,
};
use crate::services::{DownloaderService, SettingsService};

#[derive(Debug, Error)]
pub enum RssProcessingError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("RSS fetch error: {0}")]
    RssFetch(String),
    #[error("Bangumi not found: {0}")]
    BangumiNotFound(i64),
    #[error("RSS not found: {0}")]
    RssNotFound(i64),
    #[error("Path generation error: {0}")]
    PathGeneration(String),
}

/// Statistics from processing a single RSS feed
#[derive(Debug, Default)]
pub struct ProcessingStats {
    pub items_fetched: usize,
    pub items_filtered: usize,
    pub torrents_created: usize,
    pub tasks_added: usize,
    pub errors: usize,
}

/// Statistics from batch processing multiple RSS feeds
#[derive(Debug, Default)]
pub struct BatchStats {
    pub total_rss: usize,
    pub successful: usize,
    pub failed: usize,
    pub total_torrents: usize,
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
    pub async fn process_single(
        &self,
        rss: &Rss,
        global_filters: &[Regex],
    ) -> Result<ProcessingStats, RssProcessingError> {
        tracing::debug!("Processing RSS: {} (id={})", rss.url, rss.id);

        let mut stats = ProcessingStats::default();

        // Get bangumi info for path generation
        let bangumi = BangumiRepository::get_by_id(&self.db, rss.bangumi_id)
            .await?
            .ok_or(RssProcessingError::BangumiNotFound(rss.bangumi_id))?;

        // Parse URL to determine source type
        let source = parse_rss_source(&rss.url);

        // Fetch RSS feed
        let items = self
            .rss_client
            .fetch(&source)
            .await
            .map_err(|e| RssProcessingError::RssFetch(e.to_string()))?;

        stats.items_fetched = items.len();
        tracing::debug!("Fetched {} items from RSS {}", items.len(), rss.id);

        // Compile RSS-specific exclude filters
        let rss_filters = compile_filters(&rss.exclude_filters);

        for item in items {
            let title = item.title();
            let info_hash = item.info_hash();
            let torrent_url = item.torrent_url();

            // Check exclude filters (global + RSS-specific)
            if matches_any_filter(title, global_filters) || matches_any_filter(title, &rss_filters)
            {
                tracing::debug!("Filtered out by exclude filter: {}", title);
                stats.items_filtered += 1;
                continue;
            }

            // Parse title to extract episode number
            let episode_number = match self.parser.parse(title) {
                Ok(parse_result) => parse_result.episode,
                Err(e) => {
                    tracing::warn!("Failed to parse title '{}': {}", title, e);
                    None
                }
            };

            // Skip if episode number cannot be parsed
            let Some(episode) = episode_number else {
                tracing::warn!("Skipping RSS item without episode number: {}", title);
                continue;
            };

            // For non-primary RSS, filter by episode number
            if !rss.is_primary {
                // Check if this episode already exists for this bangumi
                let existing =
                    TorrentRepository::get_by_bangumi_episode(&self.db, rss.bangumi_id, episode)
                        .await?;

                if !existing.is_empty() {
                    continue;
                }
            }

            // Check if torrent already exists by info_hash
            if TorrentRepository::exists_by_info_hash(&self.db, info_hash).await? {
                continue;
            }

            // Create torrent record with parsed episode number
            let torrent = TorrentRepository::create(
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
            .await?;

            stats.torrents_created += 1;

            // Create download task
            let task = DownloadTaskRepository::create(
                &self.db,
                CreateDownloadTask {
                    torrent_id: torrent.id,
                },
            )
            .await?;

            // Use the save_path stored in bangumi (auto-generated when bangumi was created)
            // and generate filename for this specific episode
            let save_path = bangumi.save_path.clone();

            let filename = pathgen::generate_filename(
                &bangumi.title_chinese,
                bangumi.season,
                episode,
                Some(bangumi.platform.as_str()),
            );

            // Add to downloader
            let options = AddTorrentOptions::new(torrent_url)
                .save_path(&save_path)
                .rename(&filename);

            match self.downloader.add_task(options).await {
                Ok(_) => {
                    tracing::debug!("Added to downloader: {}", title);
                    stats.tasks_added += 1;
                    // Update task status to downloading
                    DownloadTaskRepository::update_status(&self.db, task.id, DownloadTaskStatus::Downloading)
                        .await?;
                }
                Err(e) => {
                    tracing::error!("Failed to add download task: {} - {}", title, e);
                    stats.errors += 1;
                    // Mark task as failed
                    DownloadTaskRepository::mark_failed(&self.db, task.id, &e.to_string()).await?;
                }
            }
        }

        Ok(stats)
    }

    /// Process multiple RSS subscriptions in batch.
    ///
    /// Used by the scheduled RSS fetch job to process all enabled subscriptions.
    pub async fn process_batch(
        &self,
        rss_list: Vec<Rss>,
        global_filters: &[Regex],
    ) -> BatchStats {
        let mut batch_stats = BatchStats {
            total_rss: rss_list.len(),
            ..Default::default()
        };

        for rss in rss_list {
            match self.process_single(&rss, global_filters).await {
                Ok(stats) => {
                    batch_stats.successful += 1;
                    batch_stats.total_torrents += stats.torrents_created;
                }
                Err(e) => {
                    tracing::error!("Failed to process RSS subscription: {} (id={}) - {}", rss.url, rss.id, e);
                    batch_stats.failed += 1;
                }
            }
        }

        batch_stats
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
            let service = RssProcessingService::new(db.clone(), rss_client, downloader, settings.clone());

            // Get global filters
            let settings_data = settings.get();
            let global_filters = compile_filters(&settings_data.filter.global_rss_filters);

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

                match service.process_single(&rss, &global_filters).await {
                    Ok(stats) => {
                        tracing::info!(
                            "RSS {} processed: fetched {} items, created {} torrents, added {} download tasks",
                            rss_id,
                            stats.items_fetched,
                            stats.torrents_created,
                            stats.tasks_added
                        );
                    }
                    Err(e) => {
                        tracing::error!("Failed to process RSS {}: {}", rss_id, e);
                    }
                }
            }

            tracing::info!("Background RSS fetch completed");
        });
    }

    /// Get compiled global filters from settings
    pub fn get_global_filters(&self) -> Vec<Regex> {
        let settings = self.settings.get();
        compile_filters(&settings.filter.global_rss_filters)
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

/// Compile filter strings into regex patterns
fn compile_filters(filters: &[String]) -> Vec<Regex> {
    filters
        .iter()
        .filter_map(|pattern| {
            Regex::new(pattern)
                .map_err(|e| {
                    tracing::warn!("Invalid exclude filter regex '{}': {}", pattern, e);
                    e
                })
                .ok()
        })
        .collect()
}

/// Check if title matches any of the exclude filters
fn matches_any_filter(title: &str, filters: &[Regex]) -> bool {
    filters.iter().any(|re| re.is_match(title))
}
