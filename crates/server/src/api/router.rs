use axum::Router;
use utoipa::OpenApi;
use utoipa_axum::{router::OpenApiRouter, routes};

use crate::{openapi::ApiDoc, state::AppState};

use super::handlers;

pub fn create_router(state: AppState) -> (Router, utoipa::openapi::OpenApi) {
    let (router, api) = OpenApiRouter::with_openapi(ApiDoc::openapi())
        .routes(routes!(handlers::search_bangumi))
        .routes(routes!(handlers::search_tmdb))
        .routes(routes!(handlers::create_bangumi))
        .with_state(state)
        .split_for_parts();

    (router, api)
}
