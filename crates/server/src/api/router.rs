use axum::{routing::get, Json, Router};
use utoipa::OpenApi;
use utoipa_axum::{router::OpenApiRouter, routes};

use crate::{openapi::ApiDoc, state::AppState};

use super::handlers;

pub fn create_router(state: AppState) -> (Router, utoipa::openapi::OpenApi) {
    let (router, api) = OpenApiRouter::with_openapi(ApiDoc::openapi())
        .routes(routes!(handlers::search_bangumi))
        .routes(routes!(handlers::search_tmdb))
        .routes(routes!(handlers::create_bangumi))
        .routes(routes!(handlers::get_episodes))
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
