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
mod update;

use serde::Deserialize;
#[cfg(feature = "openapi")]
use utoipa::IntoParams;

// Cache TTL constants (in seconds)
const MIKAN_SEARCH_CACHE_TTL: i64 = 604800; // 1 week
const MIKAN_DETAIL_CACHE_TTL: i64 = 2592000; // 30 days

/// Query parameters for keyword search
#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "openapi", derive(IntoParams))]
pub struct SearchQuery {
    /// Keyword to search
    pub keyword: String,
}

/// Query parameters for TMDB search with filters
#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "openapi", derive(IntoParams))]
pub struct TmdbSearchQuery {
    /// Keyword to search
    pub keyword: String,
}

/// Query parameters for ID lookup
#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "openapi", derive(IntoParams))]
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
pub use search::{find_metadata, get_metadata_detail, search_bgmtv, search_metadata, search_mikan, search_tmdb, DetailQuery, UnifiedSearchQuery};
pub use settings::{
    get_settings, reset_settings, test_notification, test_proxy, update_settings,
    TestNotificationRequest, TestProxyRequest,
};
pub use torrents::{delete_torrents, list_torrents, DeleteTorrentsRequest};
pub use update::{check_update, get_version, UpdateResponse};

// Re-export utoipa path structs for OpenAPI routing
#[cfg(feature = "openapi")]
mod openapi_paths {
    #[doc(hidden)]
    pub use super::bangumi::{
        __path_create_bangumi, __path_get_bangumi, __path_get_bangumi_by_id, __path_update_bangumi,
    };
    #[doc(hidden)]
    pub use super::calendar::{__path_get_calendar, __path_refresh_calendar};
    #[doc(hidden)]
    pub use super::downloader::__path_test_downloader_connection;
    #[doc(hidden)]
    pub use super::episodes::__path_get_episodes;
    #[doc(hidden)]
    pub use super::logs::{
        __path_cleanup_logs, __path_clear_all_logs, __path_get_logs, __path_stream_logs,
    };
    #[doc(hidden)]
    pub use super::metadata::{
        __path_get_metadata, __path_get_metadata_by_id, __path_update_metadata,
    };
    #[doc(hidden)]
    pub use super::mikan::__path_get_mikan_rss;
    #[doc(hidden)]
    pub use super::search::{__path_find_metadata, __path_get_metadata_detail, __path_search_bgmtv, __path_search_metadata, __path_search_mikan, __path_search_tmdb};
    #[doc(hidden)]
    pub use super::settings::{
        __path_get_settings, __path_reset_settings, __path_test_notification, __path_test_proxy,
        __path_update_settings,
    };
    #[doc(hidden)]
    pub use super::torrents::{__path_delete_torrents, __path_list_torrents};
    #[doc(hidden)]
    pub use super::update::{__path_check_update, __path_get_version};
}
#[cfg(feature = "openapi")]
pub use openapi_paths::*;
