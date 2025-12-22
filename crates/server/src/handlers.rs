use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};

use crate::{db::TodoRepository, models::CreateTodo, state::AppState};

pub async fn list_todos(State(state): State<AppState>) -> impl IntoResponse {
    match state.db.list_todos().await {
        Ok(todos) => (StatusCode::OK, Json(todos)).into_response(),
        Err(e) => {
            tracing::error!("Failed to list todos: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

pub async fn create_todo(
    State(state): State<AppState>,
    Json(input): Json<CreateTodo>,
) -> impl IntoResponse {
    match state.db.create_todo(&input).await {
        Ok(todo) => (StatusCode::CREATED, Json(todo)).into_response(),
        Err(e) => {
            tracing::error!("Failed to create todo: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

pub async fn delete_todo(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> impl IntoResponse {
    match state.db.delete_todo(id).await {
        Ok(true) => StatusCode::NO_CONTENT.into_response(),
        Ok(false) => (StatusCode::NOT_FOUND, "Todo not found").into_response(),
        Err(e) => {
            tracing::error!("Failed to delete todo: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
