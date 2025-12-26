use async_trait::async_trait;
use regex::Regex;
use rss::{RssClient, RssSource};
use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::models::{CreateDownloadTask, CreateTorrent};
use crate::models::Settings;
use crate::repositories::{BangumiRepository, DownloadTaskRepository, RssRepository, TorrentRepository};
use crate::services::{DownloaderService, SettingsService};
use downloader::AddTorrentOptions;
use parser::Parser;

/// RSS fetching job that runs every hour.
///
/// This job fetches RSS feeds from configured sources and processes new entries.
pub struct RssFetchJob {
    db: SqlitePool,
    rss_client: Arc<RssClient>,
    downloader: Arc<DownloaderService>,
    settings: Arc<SettingsService>,
    parser: Parser,
}

impl RssFetchJob {
    /// Creates a new RSS fetch job.
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

        // Get settings once
        let settings = self.settings.get();
        let global_filters = compile_filters(&settings.filter.global_rss_filters);

        for rss in rss_list {
            if let Err(e) = self.process_rss(&rss, &global_filters, &settings).await {
                tracing::error!("RSS 订阅处理失败: {} (id={}) - {}", rss.url, rss.id, e);
            }
        }

        tracing::info!("RSS fetch completed");

        Ok(())
    }
}

impl RssFetchJob {
    /// Process a single RSS subscription
    async fn process_rss(
        &self,
        rss: &crate::models::Rss,
        global_filters: &[Regex],
        settings: &Settings,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::debug!("Processing RSS: {} (id={})", rss.url, rss.id);

        // Get bangumi info for path generation
        let bangumi = BangumiRepository::get_by_id(&self.db, rss.bangumi_id)
            .await?
            .ok_or_else(|| format!("Bangumi not found: {}", rss.bangumi_id))?;

        // Parse URL to determine source type
        let source = parse_rss_source(&rss.url);

        // Fetch RSS feed
        let items = self.rss_client.fetch(&source).await?;
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
                    episode_number: episode,
                },
            )
            .await?;

            // Create download task
            let task = DownloadTaskRepository::create(
                &self.db,
                CreateDownloadTask {
                    torrent_id: torrent.id,
                },
            )
            .await?;

            // Generate save path and filename using pathgen
            let base_path = bangumi
                .save_path
                .as_ref()
                .unwrap_or(&settings.downloader.save_path);
            let save_path = pathgen::generate_directory(
                base_path,
                &bangumi.title_chinese,
                bangumi.year,
                bangumi.season,
                bangumi.tmdb_id,
                bangumi.kind.as_deref(),
            )?;
            let filename = pathgen::generate_filename(
                &bangumi.title_chinese,
                bangumi.season,
                episode,
                bangumi.kind.as_deref(),
            );

            // Add to downloader
            let options = AddTorrentOptions::new(torrent_url)
                .save_path(&save_path)
                .rename(&filename);

            match self.downloader.add_task(options).await {
                Ok(_) => {
                    tracing::debug!("Added to downloader: {}", title);
                    // Update task status to downloading
                    DownloadTaskRepository::update_status(
                        &self.db,
                        task.id,
                        crate::models::DownloadTaskStatus::Downloading,
                    )
                    .await?;
                }
                Err(e) => {
                    tracing::error!("下载任务添加失败: {} - {}", title, e);
                    // Mark task as failed
                    DownloadTaskRepository::mark_failed(&self.db, task.id, &e.to_string()).await?;
                }
            }
        }

        Ok(())
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
