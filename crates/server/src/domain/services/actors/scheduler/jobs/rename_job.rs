use async_trait::async_trait;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::services::RenameService;

/// File rename job that runs every 10 minutes.
///
/// This job scans for completed downloads pending rename
/// and renames them to Plex/Jellyfin compatible names.
pub struct RenameJob {
    rename_service: Arc<RenameService>,
}

impl RenameJob {
    /// Creates a new rename job.
    pub fn new(rename_service: Arc<RenameService>) -> Self {
        Self { rename_service }
    }
}

#[async_trait]
impl SchedulerJob for RenameJob {
    fn name(&self) -> &'static str {
        "FileRename"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(600) // Every 10 minutes
    }

    async fn execute(&self) -> JobResult {
        self.rename_service.process_all().await?;
        Ok(())
    }
}
