mod bangumi;
mod calendar;
mod downloader;
mod episodes;
mod logs;
mod mikan;
mod scan;
mod search;
mod settings;
mod torrents;
mod update;

use serde::Deserialize;

// Cache TTL constants (in seconds)
const MIKAN_SEARCH_CACHE_TTL: i64 = 604800; // 1 week
const MIKAN_DETAIL_CACHE_TTL: i64 = 2592000; // 30 days

/// Query parameters for keyword search
#[derive(Debug, Deserialize)]
pub struct SearchQuery {
    pub keyword: String,
}

/// Query parameters for TMDB search with filters
#[derive(Debug, Deserialize)]
pub struct TmdbSearchQuery {
    pub keyword: String,
}

/// Query parameters for ID lookup
#[derive(Debug, Deserialize)]
pub struct IdQuery {
    pub id: String,
}

// Re-export all handlers
pub use bangumi::{create_bangumi, get_bangumi, get_bangumi_by_id, update_bangumi};
pub use calendar::{get_calendar, refresh_calendar};
pub use downloader::{test_downloader_connection, TestDownloaderRequest};
pub use episodes::get_episodes;
pub use logs::{cleanup_logs, clear_all_logs, get_logs, stream_logs};
pub use mikan::get_mikan_rss;
pub use scan::{scan_import, ScanImportRequest, ScanImportResponse};
pub use search::{find_metadata, get_metadata_detail, search_bgmtv, search_metadata, search_metadata_all, search_mikan, search_tmdb, DetailQuery, UnifiedSearchQuery};
pub use settings::{
    get_settings, reset_settings, test_notification, test_proxy, update_settings,
    TestNotificationRequest, TestProxyRequest,
};
pub use torrents::{delete_torrents, list_torrents, DeleteTorrentsRequest};
pub use update::{check_update, get_version, perform_update, UpdateResponse};
