use axum::{routing::get, Json, Router};
use utoipa::OpenApi;
use utoipa_axum::{router::OpenApiRouter, routes};

use crate::{openapi::ApiDoc, state::AppState};

use super::handlers;

pub fn create_router(state: AppState) -> (Router, utoipa::openapi::OpenApi) {
    let (router, api) = OpenApiRouter::with_openapi(ApiDoc::openapi())
        .routes(routes!(handlers::search_bgmtv))
        .routes(routes!(handlers::search_tmdb))
        .routes(routes!(handlers::search_mikan))
        .routes(routes!(handlers::search_torrents))
        .routes(routes!(handlers::get_mikan_rss))
        .routes(routes!(handlers::get_calendar))
        .routes(routes!(handlers::refresh_calendar))
        .routes(routes!(handlers::create_bangumi))
        .routes(routes!(handlers::get_bangumi))
        .routes(routes!(handlers::get_bangumi_by_id, handlers::update_bangumi))
        .routes(routes!(handlers::get_episodes))
        .routes(routes!(handlers::get_settings))
        .routes(routes!(handlers::update_settings))
        .routes(routes!(handlers::reset_settings))
        .routes(routes!(handlers::test_proxy))
        .routes(routes!(handlers::test_notification))
        .routes(routes!(handlers::test_downloader_connection))
        .routes(routes!(handlers::get_logs, handlers::cleanup_logs))
        .routes(routes!(handlers::stream_logs))
        .routes(routes!(handlers::list_torrents))
        .routes(routes!(handlers::delete_torrents))
        .with_state(state)
        .split_for_parts();

    // Clone the API spec for the JSON endpoint
    let api_json = api.clone();

    // Add OpenAPI JSON endpoint
    let router = router.route(
        "/api/openapi.json",
        get(move || async move { Json(api_json) }),
    );

    (router, api)
}
