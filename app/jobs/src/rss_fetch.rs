//! RSS Fetch Actor
//!
//! Periodically fetches RSS feeds and processes new entries.

use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;

use domain::repositories::RssRepository;
use domain::services::{RssProcessingService, SettingsService};

use super::actor::{spawn_periodic_actor, ActorHandle, PeriodicActor};

/// RSS fetch interval (1 hour)
const FETCH_INTERVAL: Duration = Duration::from_secs(3600);

/// Handle for communicating with RssFetchActor
pub type RssFetchHandle = ActorHandle;

/// RSS Fetch Actor
///
/// Runs a background task that fetches RSS feeds at regular intervals.
struct RssFetchActor {
    db: SqlitePool,
    rss_processing: Arc<RssProcessingService>,
    settings: Arc<SettingsService>,
}

impl PeriodicActor for RssFetchActor {
    fn interval(&self) -> Duration {
        FETCH_INTERVAL
    }

    fn name(&self) -> &'static str {
        "rss_fetch"
    }

    async fn execute(&mut self) {
        tracing::debug!("RSS fetch job started");

        // Pre-check: skip if downloader is not configured
        let settings = self.settings.get();
        if !settings.downloader.is_active_config_complete() {
            tracing::debug!(
                "RSS fetch job skipped: Downloader not configured (type: {:?})",
                settings.downloader.downloader_type
            );
            return;
        }

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
    settings: Arc<SettingsService>,
) -> RssFetchHandle {
    spawn_periodic_actor(RssFetchActor {
        db,
        rss_processing,
        settings,
    })
}
