use axum::Router;

use crate::state::AppState;

use super::handlers;

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
        .route("/api/search/metadata/all", get(handlers::search_metadata_all))
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
        // Bangumi-Series linking
        .route(
            "/api/bangumi/{id}/link-series",
            post(handlers::link_bangumi_to_series).delete(handlers::unlink_bangumi_from_series),
        )
        // Episodes endpoint
        .route("/api/episodes/{subject_id}", get(handlers::get_episodes))
        // Series endpoints
        .route(
            "/api/series",
            get(handlers::get_series).post(handlers::create_series),
        )
        .route(
            "/api/series/{id}",
            get(handlers::get_series_by_id)
                .patch(handlers::update_series)
                .delete(handlers::delete_series),
        )
        .route("/api/series/{id}/refresh", post(handlers::refresh_series))
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
