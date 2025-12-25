use utoipa::OpenApi;

use crate::models::{
    BangumiWithRss, DownloaderSettings, Event, EventLevel, FilterSettings, Rss, Settings,
    UpdateBangumiRequest, UpdateDownloaderSettings, UpdateFilterSettings, UpdateSettings,
};

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
        (name = "events", description = "System events and logging endpoints")
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
        Event,
        EventLevel
    ))
)]
pub struct ApiDoc;
