//! RSS processing service.
//!
//! This module provides the RSS processing service for fetching and processing
//! anime RSS feeds. It handles:
//!
//! - Fetching RSS feeds with conditional requests (ETag/Last-Modified)
//! - Filtering items by include/exclude patterns and publication date
//! - Parsing torrent titles to extract episode information
//! - Adding download tasks for new episodes
//! - "Washing" (replacing) existing torrents with higher-priority ones
//!
//! # Usage
//!
//! ```ignore
//! let service = RssProcessingService::new(db, rss_client, downloader, settings, washing);
//! service.process_single(&rss, &[]).await;
//! ```

mod filters;
mod processing;
mod service;
mod types;

pub use service::RssProcessingService;
