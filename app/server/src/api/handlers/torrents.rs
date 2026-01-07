use axum::{extract::State, http::StatusCode, Json};
use serde::Deserialize;
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

use crate::error::AppResult;
use crate::services::Task;
use crate::state::AppState;

/// List all torrents
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/torrents",
    tag = "torrents",
    responses(
        (status = 200, description = "List of all torrents", body = Vec<Task>),
        (status = 400, description = "Downloader not configured")
    )
))]
pub async fn list_torrents(State(state): State<AppState>) -> AppResult<Json<Vec<Task>>> {
    let torrents = state.downloader.get_tasks(None).await?;
    Ok(Json(torrents))
}

/// Request to delete torrents
#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct DeleteTorrentsRequest {
    /// List of torrent hashes
    pub hashes: Vec<String>,
    /// Whether to delete downloaded files (default: true)
    #[serde(default = "default_delete_files")]
    pub delete_files: bool,
}

fn default_delete_files() -> bool {
    true
}

/// Delete torrents
#[cfg_attr(feature = "openapi", utoipa::path(
    post,
    path = "/api/torrents/delete",
    tag = "torrents",
    request_body = DeleteTorrentsRequest,
    responses(
        (status = 200, description = "Torrents deleted successfully"),
        (status = 400, description = "Invalid request (empty hashes) or downloader not configured")
    )
))]
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
        .downloader
        .delete_task(&hashes, payload.delete_files)
        .await?;
    Ok(StatusCode::OK)
}
