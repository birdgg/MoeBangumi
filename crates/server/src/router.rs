use axum::{routing::get, Router};

use crate::{handlers, state::AppState};

pub fn create_router(state: AppState) -> Router {
    Router::new()
        .route(
            "/todos",
            get(handlers::list_todos).post(handlers::create_todo),
        )
        .with_state(state)
}
