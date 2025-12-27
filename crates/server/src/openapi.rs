use utoipa::OpenApi;

use crate::api::handlers::{ControlTorrentsRequest, DeleteTorrentsRequest};
use crate::models::{
    BangumiWithRss, DownloaderSettings, FilterSettings, Log, LogLevel, Rss, Settings,
    TorrentSearchResult, TorrentSource, UpdateBangumiRequest, UpdateDownloaderSettings,
    UpdateFilterSettings, UpdateSettings,
};
use crate::services::{ServerState, SyncMainData, SyncTorrentInfo, TorrentInfo};

#[derive(OpenApi)]
#[openapi(
    info(
        title = "Moe API",
        version = "1.0.0"
    ),
    tags(
        (name = "search", description = "Bangumi search endpoints"),
        (name = "bangumi", description = "Bangumi management endpoints"),
        (name = "settings", description = "Application settings endpoints"),
        (name = "logs", description = "System logs endpoints"),
        (name = "torrents", description = "Torrent management endpoints")
    ),
    components(schemas(
        bgmtv::SearchSubjectsResponse,
        bgmtv::Subject,
        BangumiWithRss,
        Rss,
        UpdateBangumiRequest,
        Settings,
        DownloaderSettings,
        FilterSettings,
        UpdateSettings,
        UpdateDownloaderSettings,
        UpdateFilterSettings,
        Log,
        LogLevel,
        TorrentInfo,
        ControlTorrentsRequest,
        DeleteTorrentsRequest,
        SyncMainData,
        SyncTorrentInfo,
        ServerState,
        TorrentSearchResult,
        TorrentSource
    ))
)]
pub struct ApiDoc;
