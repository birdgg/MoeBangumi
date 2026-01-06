//! Utility libraries
//!
//! Contains reusable utilities:
//! - pathgen: Path generation for media files
//! - rss: RSS feed fetching and parsing
//! - season_iterator: Season date range utilities
//! - title: Title cleaning utilities for search
//! - tracing_layer: Tracing layer for database logging

pub mod pathgen;
pub mod rss;
mod season_iterator;
pub mod title;
mod tracing_layer;

pub use pathgen::{
    generate_directory, generate_filename, generate_path, DefaultFormatter, PathBuilder,
    PathFormatter, PathGenError, PathInfo, PathSanitizer,
};
pub use rss::{FetchContext, FetchResult, RssClient, RssError, RssItem, RssSource};
pub use season_iterator::SeasonIterator;
pub use title::clean_title_for_search;
pub use tracing_layer::{create_log_channel, start_log_writer, DatabaseLayer, LogReceiver};
