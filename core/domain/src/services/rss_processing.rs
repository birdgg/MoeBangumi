//! RSS processing module.
//!
//! This module handles RSS feed processing including fetching, filtering,
//! parsing, and scheduling downloads.

mod adapters;
mod filters;
mod processor;
mod service;
mod traits;

pub use adapters::{DefaultRssFetcher, DefaultTaskScheduler, SqliteRssDataAccess};
pub use filters::{filter_by_pub_date, filter_rss_items, parse_rss_source, MAX_ITEMS_PER_RSS};
pub use processor::RssProcessor;
pub use service::RssProcessingService;
pub use traits::{ProcessAction, RssDataAccess, RssFetcher, RssProcessingError, TaskScheduler};
