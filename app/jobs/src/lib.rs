//! Background jobs layer
//!
//! Contains timer-based actors that run periodically:
//! - `rss_fetch`: Fetches RSS feeds every hour
//! - `rename`: Renames completed downloads every 10 minutes
//! - `log_cleanup`: Cleans up old logs every 24 hours

mod log_cleanup;
mod rename;
mod rss_fetch;

pub use log_cleanup::{create_log_cleanup_actor, create_log_cleanup_actor_with_retention, LogCleanupHandle};
pub use rename::{create_rename_actor, RenameHandle};
pub use rss_fetch::{create_rss_fetch_actor, RssFetchHandle};
