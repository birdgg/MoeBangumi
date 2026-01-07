//! RSS Fetch Actor
//!
//! Periodically fetches RSS feeds and processes new entries.

use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio::time::MissedTickBehavior;

use domain::repositories::RssRepository;
use domain::services::RssProcessingService;

/// RSS fetch interval (1 hour)
const FETCH_INTERVAL: Duration = Duration::from_secs(3600);

/// Message types for RssFetchActor
enum Message {
    Shutdown,
}

/// Handle for communicating with RssFetchActor
#[derive(Clone)]
pub struct RssFetchHandle {
    sender: mpsc::Sender<Message>,
}

impl RssFetchHandle {
    /// Signal the actor to shutdown
    pub async fn shutdown(&self) {
        let _ = self.sender.send(Message::Shutdown).await;
    }
}

/// RSS Fetch Actor
///
/// Runs a background task that fetches RSS feeds at regular intervals.
struct RssFetchActor {
    db: SqlitePool,
    rss_processing: Arc<RssProcessingService>,
    receiver: mpsc::Receiver<Message>,
}

impl RssFetchActor {
    fn new(
        db: SqlitePool,
        rss_processing: Arc<RssProcessingService>,
        receiver: mpsc::Receiver<Message>,
    ) -> Self {
        Self {
            db,
            rss_processing,
            receiver,
        }
    }

    async fn run(mut self) {
        let mut timer = tokio::time::interval(FETCH_INTERVAL);
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
                            tracing::info!("RSS fetch actor stopped");
                            break;
                        }
                    }
                }
            }
        }
    }

    async fn execute(&self) {
        tracing::debug!("RSS fetch job started");

        // Get all enabled RSS subscriptions
        let rss_list = match RssRepository::get_enabled(&self.db).await {
            Ok(list) => list,
            Err(e) => {
                tracing::error!("Failed to get RSS subscriptions: {}", e);
                return;
            }
        };

        if rss_list.is_empty() {
            tracing::debug!("No RSS subscriptions to process");
            return;
        }

        // Get global exclude filters from settings
        let global_exclude_filters = self.rss_processing.get_global_exclude_filters();

        // Process all RSS subscriptions
        self.rss_processing
            .process_batch(rss_list, &global_exclude_filters)
            .await;

        tracing::debug!("RSS fetch job completed");
    }
}

/// Create and start the RSS fetch actor
pub fn create_rss_fetch_actor(
    db: SqlitePool,
    rss_processing: Arc<RssProcessingService>,
) -> RssFetchHandle {
    let (sender, receiver) = mpsc::channel(8);

    let actor = RssFetchActor::new(db, rss_processing, receiver);
    tokio::spawn(actor.run());

    RssFetchHandle { sender }
}
