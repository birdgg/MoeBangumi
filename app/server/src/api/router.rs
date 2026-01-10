use axum::Router;

use crate::state::AppState;

use super::handlers;

// OpenAPI mode: use OpenApiRouter with utoipa macros
#[cfg(feature = "openapi")]
pub fn create_router(state: AppState) -> (Router, utoipa::openapi::OpenApi) {
    use axum::{routing::get, Json};
    use utoipa::OpenApi;
    use utoipa_axum::{router::OpenApiRouter, routes};

    use crate::openapi::ApiDoc;

    let (router, api) = OpenApiRouter::with_openapi(ApiDoc::openapi())
        .routes(routes!(handlers::search_bgmtv))
        .routes(routes!(handlers::search_tmdb))
        .routes(routes!(handlers::search_mikan))
        .routes(routes!(handlers::search_metadata))
        .routes(routes!(handlers::find_metadata))
        .routes(routes!(handlers::get_metadata_detail))
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
        .routes(routes!(handlers::clear_all_logs))
        .routes(routes!(handlers::stream_logs))
        .routes(routes!(handlers::list_torrents))
        .routes(routes!(handlers::delete_torrents))
        // Version/update endpoints
        .routes(routes!(handlers::get_version))
        .routes(routes!(handlers::check_update))
        .routes(routes!(handlers::perform_update))
        // Scan endpoints
        .routes(routes!(handlers::scan_import))
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

// Non-OpenAPI mode: use standard axum Router
#[cfg(not(feature = "openapi"))]
pub fn create_router(state: AppState) -> Router {
    use axum::routing::{delete, get, post};

    Router::new()
        // Search endpoints
        .route("/api/search/bgmtv", get(handlers::search_bgmtv))
        .route("/api/search/tmdb", get(handlers::search_tmdb))
        .route("/api/search/mikan", get(handlers::search_mikan))
        .route("/api/search/metadata", get(handlers::search_metadata))
        .route("/api/search/metadata/find", get(handlers::find_metadata))
        .route("/api/search/metadata/detail", get(handlers::get_metadata_detail))
        // Mikan endpoints
        .route("/api/mikan/rss", get(handlers::get_mikan_rss))
        // Calendar endpoints
        .route("/api/calendar", get(handlers::get_calendar))
        .route("/api/calendar/refresh", post(handlers::refresh_calendar))
        // Bangumi endpoints
        .route(
            "/api/bangumi",
            post(handlers::create_bangumi).get(handlers::get_bangumi),
        )
        .route(
            "/api/bangumi/{id}",
            get(handlers::get_bangumi_by_id).patch(handlers::update_bangumi),
        )
        // Episodes endpoint
        .route("/api/episodes/{subject_id}", get(handlers::get_episodes))
        // Settings endpoints
        .route(
            "/api/settings",
            get(handlers::get_settings).patch(handlers::update_settings),
        )
        .route("/api/settings/reset", post(handlers::reset_settings))
        // Test endpoints
        .route("/api/proxy/test", post(handlers::test_proxy))
        .route("/api/notification/test", post(handlers::test_notification))
        .route(
            "/api/downloader/test",
            post(handlers::test_downloader_connection),
        )
        // Logs endpoints
        .route(
            "/api/logs",
            get(handlers::get_logs).delete(handlers::cleanup_logs),
        )
        .route("/api/logs/all", delete(handlers::clear_all_logs))
        .route("/api/logs/stream", get(handlers::stream_logs))
        // Torrents endpoints
        .route("/api/torrents", get(handlers::list_torrents))
        .route("/api/torrents/delete", post(handlers::delete_torrents))
        // Version/update endpoints
        .route("/api/version", get(handlers::get_version))
        .route("/api/version/check", post(handlers::check_update))
        .route("/api/version/update", post(handlers::perform_update))
        // Scan endpoints
        .route("/api/scan/import", post(handlers::scan_import))
        .with_state(state)
}
