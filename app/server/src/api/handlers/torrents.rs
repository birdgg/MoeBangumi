use axum::{extract::State, http::StatusCode, Json};
use serde::Deserialize;

use crate::error::AppResult;
use crate::services::Task;
use crate::state::AppState;

pub async fn list_torrents(State(state): State<AppState>) -> AppResult<Json<Vec<Task>>> {
    let torrents = state.services.downloader.get_tasks(None).await?;
    Ok(Json(torrents))
}

#[derive(Debug, Deserialize)]
pub struct DeleteTorrentsRequest {
    pub hashes: Vec<String>,
    #[serde(default = "default_delete_files")]
    pub delete_files: bool,
}

fn default_delete_files() -> bool {
    true
}

pub async fn delete_torrents(
    State(state): State<AppState>,
    Json(payload): Json<DeleteTorrentsRequest>,
) -> AppResult<StatusCode> {
    if payload.hashes.is_empty() {
        return Err(crate::error::AppError::bad_request(
            "At least one hash is required",
        ));
    }

    let hashes: Vec<&str> = payload.hashes.iter().map(|s| s.as_str()).collect();
    state
        .services
        .downloader
        .delete_task(&hashes, payload.delete_files)
        .await?;
    Ok(StatusCode::OK)
}
