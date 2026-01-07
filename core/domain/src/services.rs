//! Business services layer
//!
//! Contains all business logic services and actor-based services.

mod bangumi;
mod cache;
mod calendar;
mod http_client;
mod log;
mod rename;
mod rss_processing;
mod settings;
mod washing;

pub mod actors;

// Business services
pub use bangumi::{BangumiError, BangumiService};
pub use cache::{CacheError, CacheService};
pub use calendar::{CalendarError, CalendarService};
pub use http_client::{HttpClientError, HttpClientService};
pub use log::{LogError, LogService};
pub use rename::{RenameError, RenameService};
pub use rss_processing::RssProcessingService;
pub use settings::{SettingsError, SettingsService, SettingsWatcher};
pub use washing::{WashParams, WashingError, WashingService};

// Re-export actors
pub use actors::*;
