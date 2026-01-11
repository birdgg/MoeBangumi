//! RSS Processing Service.
//!
//! This module contains the core RSS processing service that handles
//! fetching, parsing, and downloading anime torrents from RSS feeds.

use futures::stream::{self, StreamExt};
use parser::Parser;
use rss::RssClient;
use sqlx::SqlitePool;
use std::collections::HashMap;
use std::sync::Arc;

use crate::models::Rss;
use crate::repositories::RssRepository;
use crate::services::washing::WashingService;
use crate::services::{DownloaderHandle, SettingsService};

/// Maximum number of RSS feeds to fetch concurrently
const RSS_FETCH_CONCURRENCY: usize = 5;

/// RSS processing service for fetching and processing anime RSS feeds.
#[derive(Clone)]
pub struct RssProcessingService {
    pub(crate) db: SqlitePool,
    pub(crate) rss_client: Arc<RssClient>,
    pub(crate) downloader: Arc<DownloaderHandle>,
    pub(crate) settings: Arc<SettingsService>,
    pub(crate) washing: Arc<WashingService>,
    pub(crate) parser: Parser,
}

impl RssProcessingService {
    /// Create a new RSS processing service.
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

    /// Spawn background tasks to process RSS subscriptions by their IDs.
    ///
    /// This method is used by BangumiService to trigger immediate RSS processing
    /// after creating or updating a bangumi with new RSS subscriptions.
    /// The processing happens asynchronously and does not block the API response.
    pub fn spawn_background(&self, rss_ids: Vec<i64>) {
        if rss_ids.is_empty() {
            return;
        }

        let service = self.clone();

        tokio::spawn(async move {
            tracing::info!(
                "Starting background RSS fetch for {} subscriptions",
                rss_ids.len()
            );

            let global_exclude_filters = service.get_global_exclude_filters();

            for rss_id in rss_ids {
                let rss = match RssRepository::get_by_id(&service.db, rss_id).await {
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

                service.process_single(&rss, &global_exclude_filters).await;
            }

            tracing::info!("Background RSS fetch completed");
        });
    }
}
