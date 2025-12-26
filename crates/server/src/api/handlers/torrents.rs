use axum::{
    extract::{Query, State},
    http::StatusCode,
    Json,
};
use serde::Deserialize;
use utoipa::{IntoParams, ToSchema};

use crate::error::AppResult;
use crate::services::{SyncMainData, TorrentInfo};
use crate::state::AppState;

/// List all torrents
#[utoipa::path(
    get,
    path = "/api/torrents",
    tag = "torrents",
    responses(
        (status = 200, description = "List of all torrents", body = Vec<TorrentInfo>),
        (status = 400, description = "Downloader not configured"),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn list_torrents(State(state): State<AppState>) -> AppResult<Json<Vec<TorrentInfo>>> {
    let torrents = state.downloader.get_tasks().await?;
    Ok(Json(torrents))
}

/// Request to control torrents (pause/resume)
#[derive(Debug, Deserialize, ToSchema)]
pub struct ControlTorrentsRequest {
    /// List of torrent hashes
    pub hashes: Vec<String>,
}

/// Pause torrents
#[utoipa::path(
    post,
    path = "/api/torrents/pause",
    tag = "torrents",
    request_body = ControlTorrentsRequest,
    responses(
        (status = 200, description = "Torrents paused successfully"),
        (status = 400, description = "Invalid request (empty hashes) or downloader not configured"),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn pause_torrents(
    State(state): State<AppState>,
    Json(payload): Json<ControlTorrentsRequest>,
) -> AppResult<StatusCode> {
    if payload.hashes.is_empty() {
        return Err(crate::error::AppError::bad_request(
            "At least one hash is required",
        ));
    }

    let hashes: Vec<&str> = payload.hashes.iter().map(|s| s.as_str()).collect();
    state.downloader.pause_task(&hashes).await?;
    Ok(StatusCode::OK)
}

/// Resume torrents
#[utoipa::path(
    post,
    path = "/api/torrents/resume",
    tag = "torrents",
    request_body = ControlTorrentsRequest,
    responses(
        (status = 200, description = "Torrents resumed successfully"),
        (status = 400, description = "Invalid request (empty hashes) or downloader not configured"),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn resume_torrents(
    State(state): State<AppState>,
    Json(payload): Json<ControlTorrentsRequest>,
) -> AppResult<StatusCode> {
    if payload.hashes.is_empty() {
        return Err(crate::error::AppError::bad_request(
            "At least one hash is required",
        ));
    }

    let hashes: Vec<&str> = payload.hashes.iter().map(|s| s.as_str()).collect();
    state.downloader.resume_task(&hashes).await?;
    Ok(StatusCode::OK)
}

/// Request to delete torrents
#[derive(Debug, Deserialize, ToSchema)]
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
#[utoipa::path(
    post,
    path = "/api/torrents/delete",
    tag = "torrents",
    request_body = DeleteTorrentsRequest,
    responses(
        (status = 200, description = "Torrents deleted successfully"),
        (status = 400, description = "Invalid request (empty hashes) or downloader not configured"),
        (status = 500, description = "Internal server error")
    )
)]
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

/// Query parameters for sync maindata
#[derive(Debug, Deserialize, IntoParams)]
pub struct SyncMainDataQuery {
    /// Response ID from previous request. Use 0 for initial request.
    #[serde(default)]
    pub rid: i64,
}

/// Sync maindata for incremental torrent updates
///
/// Returns full torrent data on first call (rid=0), then incremental changes on subsequent calls.
/// Use the returned `rid` value in the next request to get only changed data.
#[utoipa::path(
    get,
    path = "/api/torrents/sync",
    tag = "torrents",
    params(SyncMainDataQuery),
    responses(
        (status = 200, description = "Sync maindata with torrent updates", body = SyncMainData),
        (status = 400, description = "Downloader not configured"),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn sync_maindata(
    State(state): State<AppState>,
    Query(params): Query<SyncMainDataQuery>,
) -> AppResult<Json<SyncMainData>> {
    let data = state.downloader.get_tasks_info(params.rid).await?;
    Ok(Json(data))
}
