use async_trait::async_trait;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::services::MikanMappingService;

/// Mikan mapping sync job that runs weekly.
///
/// This job fetches the current season's bangumi list from Mikan
/// and establishes mikan_id -> bgmtv_id mappings for quick lookup.
pub struct MikanMappingSyncJob {
    mapping_service: Arc<MikanMappingService>,
}

impl MikanMappingSyncJob {
    /// Creates a new Mikan mapping sync job.
    pub fn new(mapping_service: Arc<MikanMappingService>) -> Self {
        Self { mapping_service }
    }
}

#[async_trait]
impl SchedulerJob for MikanMappingSyncJob {
    fn name(&self) -> &'static str {
        "MikanMappingSync"
    }

    fn interval(&self) -> Duration {
        // Run every 7 days (weekly)
        Duration::from_secs(7 * 24 * 3600)
    }

    async fn execute(&self) -> JobResult {
        tracing::info!("Starting Mikan mapping sync job");

        match self.mapping_service.sync_current_season().await {
            Ok(count) => {
                tracing::info!(
                    "Mikan mapping sync completed: {} new mappings added",
                    count
                );
                Ok(())
            }
            Err(e) => {
                tracing::error!("Mikan mapping sync failed: {}", e);
                Err(Box::new(e))
            }
        }
    }
}
