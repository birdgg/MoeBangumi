use axum::Router;
use utoipa::OpenApi;
use utoipa_axum::{router::OpenApiRouter, routes};

use crate::{handlers, openapi::ApiDoc, state::AppState};

pub fn create_router(state: AppState) -> (Router, utoipa::openapi::OpenApi) {
    let (router, api) = OpenApiRouter::with_openapi(ApiDoc::openapi())
        .routes(routes!(handlers::list_todos, handlers::create_todo))
        .with_state(state)
        .split_for_parts();

    (router, api)
}
