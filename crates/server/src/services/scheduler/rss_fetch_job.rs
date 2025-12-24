use async_trait::async_trait;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};

/// RSS fetching job that runs every minute.
///
/// This job fetches RSS feeds from configured sources and processes new entries.
pub struct RssFetchJob {
    // TODO: Add dependencies when implementing
    // db: SqlitePool,
    // mikan: Arc<MikanClient>,
}

impl RssFetchJob {
    /// Creates a new RSS fetch job.
    pub fn new() -> Self {
        Self {
            // TODO: Initialize with dependencies
        }
    }
}

impl Default for RssFetchJob {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl SchedulerJob for RssFetchJob {
    fn name(&self) -> &'static str {
        "RssFetch"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(60) // Every minute
    }

    async fn execute(&self) -> JobResult {
        tracing::debug!("Executing RSS fetch job");

        // TODO: Implement RSS fetching logic
        // 1. Get enabled RSS subscriptions from database
        // 2. Fetch RSS feeds
        // 3. Parse and process new entries
        // 4. Update database with new items

        Ok(())
    }
}
