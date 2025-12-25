use async_trait::async_trait;
use regex::Regex;
use rss::{RssClient, RssSource};
use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::models::{CreateDownloadTask, CreateTorrent};
use crate::repositories::{DownloadTaskRepository, RssRepository, TorrentRepository};
use crate::services::DownloaderService;
use downloader::AddTorrentOptions;
use parser::Parser;

/// RSS fetching job that runs every hour.
///
/// This job fetches RSS feeds from configured sources and processes new entries.
pub struct RssFetchJob {
    db: SqlitePool,
    rss_client: Arc<RssClient>,
    downloader: Arc<DownloaderService>,
    parser: Parser,
}

impl RssFetchJob {
    /// Creates a new RSS fetch job.
    pub fn new(
        db: SqlitePool,
        rss_client: Arc<RssClient>,
        downloader: Arc<DownloaderService>,
    ) -> Self {
        Self {
            db,
            rss_client,
            downloader,
            parser: Parser::new(),
        }
    }
}

#[async_trait]
impl SchedulerJob for RssFetchJob {
    fn name(&self) -> &'static str {
        "RssFetch"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(3600) // Every hour
    }

    async fn execute(&self) -> JobResult {
        tracing::info!("Starting RSS fetch job");

        // Get all enabled RSS subscriptions
        let rss_list = RssRepository::get_enabled(&self.db).await?;

        if rss_list.is_empty() {
            tracing::debug!("No enabled RSS subscriptions found");
            return Ok(());
        }

        tracing::info!("Found {} enabled RSS subscriptions", rss_list.len());

        let mut total_new = 0;
        let mut total_skipped = 0;
        let mut total_filtered = 0;

        for rss in rss_list {
            match self.process_rss(&rss).await {
                Ok((new, skipped, filtered)) => {
                    total_new += new;
                    total_skipped += skipped;
                    total_filtered += filtered;
                }
                Err(e) => {
                    tracing::error!("RSS 订阅处理失败: {} (id={}) - {}", rss.url, rss.id, e);
                    // Continue processing other RSS feeds
                }
            }
        }

        tracing::info!(
            "RSS fetch completed: {} new torrents, {} skipped (duplicate), {} filtered",
            total_new,
            total_skipped,
            total_filtered
        );

        Ok(())
    }
}

impl RssFetchJob {
    /// Process a single RSS subscription
    async fn process_rss(
        &self,
        rss: &crate::models::Rss,
    ) -> Result<(usize, usize, usize), Box<dyn std::error::Error + Send + Sync>> {
        tracing::debug!("Processing RSS: {} (id={})", rss.url, rss.id);

        // Parse URL to determine source type
        let source = parse_rss_source(&rss.url);

        // Fetch RSS feed
        let items = self.rss_client.fetch(&source).await?;
        tracing::debug!("Fetched {} items from RSS {}", items.len(), rss.id);

        // Compile exclude filters
        let filters = compile_filters(&rss.exclude_filters);

        let mut new_count = 0;
        let mut skipped_count = 0;
        let mut filtered_count = 0;

        for item in items {
            let title = item.title();
            let info_hash = item.info_hash();
            let torrent_url = item.torrent_url();

            // Check exclude filters
            if matches_any_filter(title, &filters) {
                tracing::debug!("Filtered out by exclude filter: {}", title);
                filtered_count += 1;
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
                filtered_count += 1;
                continue;
            };

            // For non-primary RSS, filter by episode number
            if !rss.is_primary {
                // Check if this episode already exists for this bangumi
                let existing = TorrentRepository::get_by_bangumi_episode(
                    &self.db,
                    rss.bangumi_id,
                    episode,
                )
                .await?;

                if !existing.is_empty() {
                    tracing::debug!(
                        "Filtered out: episode {} already exists for bangumi {} (title: {})",
                        episode,
                        rss.bangumi_id,
                        title
                    );
                    filtered_count += 1;
                    continue;
                }
            }

            // Check if torrent already exists by info_hash
            if TorrentRepository::exists_by_info_hash(&self.db, info_hash).await? {
                tracing::debug!("Skipping duplicate info_hash: {}", title);
                skipped_count += 1;
                continue;
            }

            // Create torrent record with parsed episode number
            let torrent = TorrentRepository::create(
                &self.db,
                CreateTorrent {
                    bangumi_id: rss.bangumi_id,
                    rss_id: Some(rss.id),
                    info_hash: info_hash.to_string(),
                    episode_number: episode,
                },
            )
            .await?;

            tracing::info!(
                "New torrent: {} (id={}, episode={})",
                title,
                torrent.id,
                torrent.episode_number
            );

            // Create download task
            let task = DownloadTaskRepository::create(
                &self.db,
                CreateDownloadTask {
                    torrent_id: torrent.id,
                },
            )
            .await?;

            // Add to downloader
            let options = AddTorrentOptions::new(torrent_url);

            match self.downloader.add_torrent(options).await {
                Ok(_) => {
                    tracing::debug!("Added to downloader: {}", title);
                    // Update task status to downloading
                    DownloadTaskRepository::update_status(
                        &self.db,
                        task.id,
                        crate::models::DownloadTaskStatus::Downloading,
                    )
                    .await?;
                    new_count += 1;
                }
                Err(e) => {
                    tracing::error!("下载任务添加失败: {} - {}", title, e);
                    // Mark task as failed
                    DownloadTaskRepository::mark_failed(&self.db, task.id, &e.to_string()).await?;
                }
            }
        }

        Ok((new_count, skipped_count, filtered_count))
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
