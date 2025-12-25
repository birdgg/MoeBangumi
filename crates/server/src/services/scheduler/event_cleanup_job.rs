use async_trait::async_trait;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::services::EventService;

/// Default retention period in days
const DEFAULT_RETENTION_DAYS: i64 = 30;

/// Event cleanup job that runs daily to remove old events.
pub struct EventCleanupJob {
    events: Arc<EventService>,
    retention_days: i64,
}

impl EventCleanupJob {
    /// Creates a new event cleanup job with default retention (30 days).
    pub fn new(events: Arc<EventService>) -> Self {
        Self {
            events,
            retention_days: DEFAULT_RETENTION_DAYS,
        }
    }

    /// Creates a new event cleanup job with custom retention period.
    pub fn with_retention(events: Arc<EventService>, retention_days: i64) -> Self {
        Self {
            events,
            retention_days,
        }
    }
}

#[async_trait]
impl SchedulerJob for EventCleanupJob {
    fn name(&self) -> &'static str {
        "EventCleanup"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(86400) // Every 24 hours
    }

    async fn execute(&self) -> JobResult {
        tracing::info!(
            "Running event cleanup job (retention: {} days)",
            self.retention_days
        );

        match self.events.cleanup(self.retention_days).await {
            Ok(deleted) => {
                if deleted > 0 {
                    tracing::info!("Event cleanup completed: {} events deleted", deleted);
                } else {
                    tracing::debug!("Event cleanup completed: no old events to delete");
                }
            }
            Err(e) => {
                tracing::error!("Event cleanup failed: {}", e);
            }
        }

        Ok(())
    }
}
