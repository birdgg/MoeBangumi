use utoipa::OpenApi;

use crate::models::{Settings, UpdateSettings};

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
        UpdateSettings
    ))
)]
pub struct ApiDoc;
