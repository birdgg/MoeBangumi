use async_trait::async_trait;
use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::repositories::RssRepository;
use crate::services::RssProcessingService;

/// RSS fetching job that runs every hour.
///
/// This job fetches RSS feeds from configured sources and processes new entries.
/// It delegates the actual processing logic to RssProcessingService.
pub struct RssFetchJob {
    db: SqlitePool,
    rss_processing: Arc<RssProcessingService>,
}

impl RssFetchJob {
    /// Creates a new RSS fetch job.
    pub fn new(db: SqlitePool, rss_processing: Arc<RssProcessingService>) -> Self {
        Self { db, rss_processing }
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

        // Get global filters from settings
        let global_filters = self.rss_processing.get_global_filters();

        // Process all RSS subscriptions using the processing service
        let stats = self.rss_processing.process_batch(rss_list, &global_filters).await;

        tracing::info!(
            "RSS fetch completed: {} successful, {} failed, {} torrents created",
            stats.successful,
            stats.failed,
            stats.total_torrents
        );

        Ok(())
    }
}
