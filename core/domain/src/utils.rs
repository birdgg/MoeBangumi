//! Utility libraries
//!
//! Contains reusable utilities:
//! - pathgen: Path generation for media files (external crate)
//! - rss: RSS feed fetching and parsing (external crate)
//! - title: Title cleaning utilities for search
//! - tracing_layer: Tracing layer for database logging

pub mod title;
mod tracing_layer;

// Re-export from external pathgen crate
pub use pathgen::{
    generate_directory, generate_filename, generate_path, DefaultFormatter, PathBuilder,
    PathFormatter, PathGenError, PathInfo, PathSanitizer,
};
// Re-export from external rss crate
pub use rss::{ClientProvider, FetchContext, FetchResult, RssClient, RssError, RssItem, RssSource};
pub use mikan::SeasonIterator;
pub use title::clean_title_for_search;
pub use tracing_layer::{create_log_channel, start_log_writer, DatabaseLayer, LogReceiver};
