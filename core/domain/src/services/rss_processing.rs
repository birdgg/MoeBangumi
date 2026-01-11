//! RSS processing service with dependency injection support.
//!
//! This module provides both the concrete `RssProcessingService` (for production use)
//! and the generic `GenericRssProcessingService` (for testing with mocked dependencies).
//!
//! # Architecture
//!
//! The RSS processing service is built on a trait abstraction layer that separates
//! the core business logic from external dependencies:
//!
//! - **traits.rs**: Defines trait interfaces for all dependencies (repositories, fetcher, etc.)
//! - **adapters.rs**: Implements traits for production types (wraps existing implementations)
//! - **service.rs**: Contains the generic service implementation with trait bounds
//!
//! # Testing
//!
//! For testing, create mock implementations of the traits and construct a
//! `GenericRssProcessingService` with those mocks. See the `mocks` module for examples.
//!
//! # Production Usage
//!
//! Use the `RssProcessingService` type alias which is pre-configured with production adapters:
//!
//! ```ignore
//! let service = RssProcessingService::new(db, rss_client, downloader, settings, washing);
//! service.process_single(&rss, &[]).await;
//! ```

mod adapters;
mod service;
mod traits;

#[cfg(test)]
pub mod mocks;

// Re-export traits for testing
pub use traits::*;

// Re-export the generic service for advanced use cases
pub use service::GenericRssProcessingService;

// Re-export adapters for construction
pub use adapters::*;

use rss::RssClient;
use sqlx::SqlitePool;
use std::sync::Arc;

use crate::services::washing::WashingService;
use crate::services::{DownloaderHandle, SettingsService};

// ============================================================================
// Production Type Alias
// ============================================================================

/// Production RSS processing service using concrete adapter types.
///
/// This type alias configures `GenericRssProcessingService` with all production
/// implementations. Use this in production code. For testing, use
/// `GenericRssProcessingService` directly with mocked dependencies.
pub type RssProcessingService = GenericRssProcessingService<
    BangumiRepositoryAdapter,
    RssRepositoryAdapter,
    TorrentRepositoryAdapter,
    RssClientAdapter,
    DownloaderAdapter,
    WashingServiceAdapter,
    SettingsAdapter,
>;

impl RssProcessingService {
    /// Create a new RSS processing service with production dependencies.
    ///
    /// This is the standard constructor for production use. It wraps all
    /// dependencies in their respective adapters internally.
    pub fn new(
        db: SqlitePool,
        rss_client: Arc<RssClient>,
        downloader: Arc<DownloaderHandle>,
        settings: Arc<SettingsService>,
        washing: Arc<WashingService>,
    ) -> Self {
        GenericRssProcessingService::with_deps(
            BangumiRepositoryAdapter::new(db.clone()),
            RssRepositoryAdapter::new(db.clone()),
            TorrentRepositoryAdapter::new(db),
            RssClientAdapter::new(rss_client),
            DownloaderAdapter::new(downloader),
            WashingServiceAdapter::new(washing),
            SettingsAdapter::new(settings),
        )
    }

    /// Spawn background tasks to process RSS subscriptions by their IDs.
    ///
    /// This method is used by BangumiService to trigger immediate RSS processing
    /// after creating or updating a bangumi with new RSS subscriptions.
    /// The processing happens asynchronously and does not block the API response.
    pub fn spawn_background(&self, rss_ids: Vec<i64>) {
        if rss_ids.is_empty() {
            return;
        }

        let service = self.clone();

        tokio::spawn(async move {
            tracing::info!(
                "Starting background RSS fetch for {} subscriptions",
                rss_ids.len()
            );

            let global_exclude_filters = service.get_global_exclude_filters();

            for rss_id in rss_ids {
                let rss = match service.rss_repo.get_by_id(rss_id).await {
                    Ok(Some(rss)) => rss,
                    Ok(None) => {
                        tracing::warn!("RSS {} not found, skipping", rss_id);
                        continue;
                    }
                    Err(e) => {
                        tracing::error!("Failed to fetch RSS {}: {}", rss_id, e);
                        continue;
                    }
                };

                service.process_single(&rss, &global_exclude_filters).await;
            }

            tracing::info!("Background RSS fetch completed");
        });
    }
}
