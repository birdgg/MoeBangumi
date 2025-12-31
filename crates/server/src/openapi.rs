use utoipa::OpenApi;

use crate::api::handlers::DeleteTorrentsRequest;
use crate::models::{
    Bangumi, BangumiWithMetadata, BangumiWithRss, CreateBangumi, CreateMetadata,
    DownloaderSettings, FilterSettings, Log, LogLevel, Metadata, Platform, PrioritySettings, Rss,
    RssEntry, Settings, SourceType, TorrentSearchResult, TorrentSource, UpdateBangumiRequest,
    UpdateDownloaderSettings, UpdateFilterSettings, UpdatePrioritySettings, UpdateSettings,
};
use crate::services::{Task, TaskStatus};

#[derive(OpenApi)]
#[openapi(
    info(
        title = "Moe API",
        version = "1.0.0"
    ),
    tags(
        (name = "search", description = "Bangumi search endpoints"),
        (name = "bangumi", description = "Bangumi management endpoints"),
        (name = "calendar", description = "Anime calendar endpoints"),
        (name = "settings", description = "Application settings endpoints"),
        (name = "logs", description = "System logs endpoints"),
        (name = "torrents", description = "Torrent management endpoints")
    ),
    components(schemas(
        bgmtv::SearchSubjectsResponse,
        bgmtv::Subject,
        bgmtv::CalendarDay,
        bgmtv::CalendarSubject,
        bgmtv::CalendarRating,
        bgmtv::CalendarCollection,
        bgmtv::Weekday,
        bgmtv::SubjectImages,
        bgmtv::SubjectType,
        Metadata,
        CreateMetadata,
        Bangumi,
        BangumiWithMetadata,
        BangumiWithRss,
        CreateBangumi,
        RssEntry,
        Rss,
        SourceType,
        UpdateBangumiRequest,
        Settings,
        DownloaderSettings,
        FilterSettings,
        PrioritySettings,
        UpdateSettings,
        UpdateDownloaderSettings,
        UpdateFilterSettings,
        UpdatePrioritySettings,
        Log,
        LogLevel,
        Platform,
        Task,
        TaskStatus,
        DeleteTorrentsRequest,
        TorrentSearchResult,
        TorrentSource
    ))
)]
pub struct ApiDoc;
