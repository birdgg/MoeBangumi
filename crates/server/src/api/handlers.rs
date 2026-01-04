mod bangumi;
mod calendar;
mod downloader;
mod episodes;
mod logs;
mod metadata;
mod mikan;
mod search;
mod settings;
mod torrents;

use serde::Deserialize;
use utoipa::IntoParams;

// Cache TTL constants (in seconds)
const MIKAN_SEARCH_CACHE_TTL: i64 = 604800; // 1 week
const MIKAN_DETAIL_CACHE_TTL: i64 = 2592000; // 30 days

/// Query parameters for keyword search
#[derive(Debug, Deserialize, IntoParams)]
pub struct SearchQuery {
    /// Keyword to search
    pub keyword: String,
}

/// Query parameters for TMDB search with filters
#[derive(Debug, Deserialize, IntoParams)]
pub struct TmdbSearchQuery {
    /// Keyword to search
    pub keyword: String,
}

/// Query parameters for ID lookup
#[derive(Debug, Deserialize, IntoParams)]
pub struct IdQuery {
    /// ID to lookup
    pub id: String,
}

// Re-export all handlers
pub use bangumi::{create_bangumi, get_bangumi, get_bangumi_by_id, update_bangumi};
pub use calendar::{get_calendar, refresh_calendar};
pub use downloader::{test_downloader_connection, TestDownloaderRequest};
pub use episodes::get_episodes;
pub use logs::{cleanup_logs, clear_all_logs, get_logs, stream_logs};
pub use metadata::{get_metadata, get_metadata_by_id, update_metadata};
pub use mikan::get_mikan_rss;
pub use search::{search_bgmtv, search_mikan, search_tmdb};
pub use settings::{
    get_settings, reset_settings, test_notification, test_proxy, update_settings,
    TestNotificationRequest, TestProxyRequest,
};
pub use torrents::{delete_torrents, list_torrents, DeleteTorrentsRequest};

// Re-export utoipa path structs for OpenAPI routing
#[doc(hidden)]
pub use bangumi::{
    __path_create_bangumi, __path_get_bangumi, __path_get_bangumi_by_id, __path_update_bangumi,
};
#[doc(hidden)]
pub use calendar::{__path_get_calendar, __path_refresh_calendar};
#[doc(hidden)]
pub use downloader::__path_test_downloader_connection;
#[doc(hidden)]
pub use episodes::__path_get_episodes;
#[doc(hidden)]
pub use logs::{__path_cleanup_logs, __path_clear_all_logs, __path_get_logs, __path_stream_logs};
#[doc(hidden)]
pub use metadata::{__path_get_metadata, __path_get_metadata_by_id, __path_update_metadata};
#[doc(hidden)]
pub use mikan::__path_get_mikan_rss;
#[doc(hidden)]
pub use search::{__path_search_bgmtv, __path_search_mikan, __path_search_tmdb};
#[doc(hidden)]
pub use settings::{
    __path_get_settings, __path_reset_settings, __path_test_notification, __path_test_proxy,
    __path_update_settings,
};
#[doc(hidden)]
pub use torrents::{__path_delete_torrents, __path_list_torrents};
