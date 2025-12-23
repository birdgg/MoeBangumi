use utoipa::OpenApi;

#[derive(OpenApi)]
#[openapi(
    info(
        title = "Moe API",
        version = "1.0.0"
    ),
    tags(
        (name = "search", description = "Bangumi search endpoints")
    ),
    components(schemas(bgmtv::SearchSubjectsResponse, bgmtv::Subject))
)]
pub struct ApiDoc;
