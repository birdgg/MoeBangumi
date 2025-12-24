use utoipa::OpenApi;

use crate::models::{
    DownloaderSettings, FilterSettings, Settings, UpdateDownloaderSettings, UpdateFilterSettings,
    UpdateSettings,
};

#[derive(OpenApi)]
#[openapi(
    info(
        title = "Moe API",
        version = "1.0.0"
    ),
    tags(
        (name = "search", description = "Bangumi search endpoints"),
        (name = "settings", description = "Application settings endpoints")
    ),
    components(schemas(
        bgmtv::SearchSubjectsResponse,
        bgmtv::Subject,
        Settings,
        DownloaderSettings,
        FilterSettings,
        UpdateSettings,
        UpdateDownloaderSettings,
        UpdateFilterSettings
    ))
)]
pub struct ApiDoc;
