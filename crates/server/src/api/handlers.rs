mod bangumi;
mod downloader;
mod episodes;
mod logs;
mod mikan;
mod scheduler;
mod search;
mod settings;
mod torrent_search;
mod torrents;
mod webhook;

use serde::Deserialize;
use utoipa::IntoParams;

use crate::models::TorrentSource;

// Cache TTL constants (in seconds)
const MIKAN_SEARCH_CACHE_TTL: i64 = 604800; // 1 week
const MIKAN_DETAIL_CACHE_TTL: i64 = 2592000; // 30 days
const TORRENT_SEARCH_CACHE_TTL: i64 = 3600; // 1 hour

/// Query parameters for keyword search
#[derive(Debug, Deserialize, IntoParams)]
pub struct SearchQuery {
    /// Keyword to search
    pub keyword: String,
}

/// Query parameters for torrent search
#[derive(Debug, Deserialize, IntoParams)]
pub struct TorrentSearchQuery {
    /// Keyword to search
    pub keyword: String,
    /// Source to search from: "nyaa" (default) or "mikan"
    #[serde(default)]
    pub source: TorrentSource,
}

/// Query parameters for ID lookup
#[derive(Debug, Deserialize, IntoParams)]
pub struct IdQuery {
    /// ID to lookup
    pub id: String,
}

// Re-export all handlers
pub use bangumi::{create_bangumi, get_bangumi, get_bangumi_by_id, update_bangumi};
pub use downloader::{test_downloader_connection, TestDownloaderRequest};
pub use episodes::get_episodes;
pub use logs::{cleanup_logs, get_logs, stream_logs};
pub use mikan::get_mikan_rss;
pub use scheduler::trigger_rss_fetch;
pub use search::{search_bgmtv, search_mikan, search_tmdb};
pub use settings::{get_settings, reset_settings, test_proxy, update_settings, TestProxyRequest};
pub use torrent_search::search_torrents;
pub use webhook::torrent_completed;
pub use torrents::{
    delete_torrents, list_torrents, pause_torrents, resume_torrents, sync_maindata,
    ControlTorrentsRequest, DeleteTorrentsRequest, SyncMainDataQuery,
};

// Re-export utoipa path structs for OpenAPI routing
#[doc(hidden)]
pub use bangumi::{__path_create_bangumi, __path_get_bangumi, __path_get_bangumi_by_id, __path_update_bangumi};
#[doc(hidden)]
pub use downloader::__path_test_downloader_connection;
#[doc(hidden)]
pub use episodes::__path_get_episodes;
#[doc(hidden)]
pub use logs::{__path_cleanup_logs, __path_get_logs, __path_stream_logs};
#[doc(hidden)]
pub use mikan::__path_get_mikan_rss;
#[doc(hidden)]
pub use search::{__path_search_bgmtv, __path_search_mikan, __path_search_tmdb};
#[doc(hidden)]
pub use scheduler::__path_trigger_rss_fetch;
#[doc(hidden)]
pub use torrent_search::__path_search_torrents;
#[doc(hidden)]
pub use settings::{__path_get_settings, __path_reset_settings, __path_test_proxy, __path_update_settings};
#[doc(hidden)]
pub use webhook::__path_torrent_completed;
#[doc(hidden)]
pub use torrents::{
    __path_delete_torrents, __path_list_torrents, __path_pause_torrents,
    __path_resume_torrents, __path_sync_maindata,
};
