use utoipa::OpenApi;

use crate::api::handlers::{DeleteTorrentsRequest, UpdateResponse};
use crate::models::{
    Bangumi, BangumiWithRss, BangumiWithSeries, CalendarDay, CalendarSubject, CreateBangumi,
    DownloaderSettings, FilterSettings, Log, LogLevel, Platform, PrioritySettings, Rss, RssEntry,
    Series, Settings, SourceType, UpdateBangumiRequest, UpdateDownloaderSettings,
    UpdateFilterSettings, UpdatePrioritySettings, UpdateSettings, Weekday,
};
use crate::services::{Task, TaskStatus};
use updater::{UpdateStatus, VersionInfo};

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
        (name = "torrents", description = "Torrent management endpoints"),
        (name = "system", description = "System version and update endpoints")
    ),
    components(schemas(
        bgmtv::SearchSubjectsResponse,
        bgmtv::Subject,
        bgmtv::SubjectImages,
        bgmtv::SubjectType,
        mikan::Season,
        CalendarDay,
        CalendarSubject,
        Weekday,
        Bangumi,
        BangumiWithSeries,
        BangumiWithRss,
        CreateBangumi,
        RssEntry,
        Rss,
        Series,
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
        VersionInfo,
        UpdateStatus,
        UpdateResponse
    ))
)]
pub struct ApiDoc;
