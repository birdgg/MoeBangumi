use async_trait::async_trait;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::services::MetadataService;

/// Metadata finish status checking job that runs every 24 hours.
///
/// This job checks unfinished metadata entries against BGM.tv episodes API
/// to determine if the anime has finished airing based on the last main episode's air date.
/// All business logic is delegated to MetadataService for better testability and reusability.
pub struct MetadataFinishCheckJob {
    metadata_service: Arc<MetadataService>,
}

impl MetadataFinishCheckJob {
    /// Creates a new metadata finish check job.
    pub fn new(metadata_service: Arc<MetadataService>) -> Self {
        Self { metadata_service }
    }
}

#[async_trait]
impl SchedulerJob for MetadataFinishCheckJob {
    fn name(&self) -> &'static str {
        "MetadataFinishCheck"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(86400) // 24 hours
    }

    async fn execute(&self) -> JobResult {
        tracing::info!("Starting metadata finish status check");

        let (updated, total, errors) = self.metadata_service.batch_check_finish_status().await?;

        tracing::info!(
            "Finish check completed: {}/{} updated, {} errors",
            updated,
            total,
            errors
        );

        Ok(())
    }
}
