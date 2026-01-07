//! Log Cleanup Actor
//!
//! Periodically cleans up old logs from the database.

use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio::time::MissedTickBehavior;

use domain::services::LogService;

/// Cleanup interval (24 hours)
const CLEANUP_INTERVAL: Duration = Duration::from_secs(86400);

/// Default retention period in days
const DEFAULT_RETENTION_DAYS: i64 = 30;

/// Message types for LogCleanupActor
enum Message {
    Shutdown,
}

/// Handle for communicating with LogCleanupActor
#[derive(Clone)]
pub struct LogCleanupHandle {
    sender: mpsc::Sender<Message>,
}

impl LogCleanupHandle {
    /// Signal the actor to shutdown
    pub async fn shutdown(&self) {
        let _ = self.sender.send(Message::Shutdown).await;
    }
}

/// Log Cleanup Actor
///
/// Runs a background task that cleans up old logs at regular intervals.
struct LogCleanupActor {
    logs: Arc<LogService>,
    retention_days: i64,
    receiver: mpsc::Receiver<Message>,
}

impl LogCleanupActor {
    fn new(logs: Arc<LogService>, retention_days: i64, receiver: mpsc::Receiver<Message>) -> Self {
        Self {
            logs,
            retention_days,
            receiver,
        }
    }

    async fn run(mut self) {
        let mut timer = tokio::time::interval(CLEANUP_INTERVAL);
        timer.set_missed_tick_behavior(MissedTickBehavior::Skip);

        // Skip first tick (immediate)
        timer.tick().await;

        loop {
            tokio::select! {
                _ = timer.tick() => {
                    self.execute().await;
                }
                msg = self.receiver.recv() => {
                    match msg {
                        Some(Message::Shutdown) | None => {
                            tracing::info!("Log cleanup actor stopped");
                            break;
                        }
                    }
                }
            }
        }
    }

    async fn execute(&self) {
        tracing::debug!("Log cleanup job started");

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
    }
}

/// Create and start the log cleanup actor with default retention (30 days)
pub fn create_log_cleanup_actor(logs: Arc<LogService>) -> LogCleanupHandle {
    create_log_cleanup_actor_with_retention(logs, DEFAULT_RETENTION_DAYS)
}

/// Create and start the log cleanup actor with custom retention period
pub fn create_log_cleanup_actor_with_retention(
    logs: Arc<LogService>,
    retention_days: i64,
) -> LogCleanupHandle {
    let (sender, receiver) = mpsc::channel(8);

    let actor = LogCleanupActor::new(logs, retention_days, receiver);
    tokio::spawn(actor.run());

    LogCleanupHandle { sender }
}
