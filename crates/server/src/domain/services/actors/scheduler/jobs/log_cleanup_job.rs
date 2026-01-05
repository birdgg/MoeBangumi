use async_trait::async_trait;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::services::LogService;

/// Default retention period in days
const DEFAULT_RETENTION_DAYS: i64 = 30;

/// Log cleanup job that runs daily to remove old logs.
pub struct LogCleanupJob {
    logs: Arc<LogService>,
    retention_days: i64,
}

impl LogCleanupJob {
    /// Creates a new log cleanup job with default retention (30 days).
    pub fn new(logs: Arc<LogService>) -> Self {
        Self {
            logs,
            retention_days: DEFAULT_RETENTION_DAYS,
        }
    }

    /// Creates a new log cleanup job with custom retention period.
    pub fn with_retention(logs: Arc<LogService>, retention_days: i64) -> Self {
        Self {
            logs,
            retention_days,
        }
    }
}

#[async_trait]
impl SchedulerJob for LogCleanupJob {
    fn name(&self) -> &'static str {
        "LogCleanup"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(86400) // Every 24 hours
    }

    async fn execute(&self) -> JobResult {
        tracing::info!(
            "Running log cleanup job (retention: {} days)",
            self.retention_days
        );

        match self.logs.cleanup(self.retention_days).await {
            Ok(deleted) => {
                if deleted > 0 {
                    tracing::info!("Log cleanup completed: {} logs deleted", deleted);
                } else {
                    tracing::debug!("Log cleanup completed: no old logs to delete");
                }
            }
            Err(e) => {
                tracing::error!("Log cleanup failed: {}", e);
            }
        }

        Ok(())
    }
}
