//! Update API handlers

use axum::{extract::State, Json};
use serde::Serialize;
use updater::VersionInfo;

use crate::error::AppResult;
use crate::state::AppState;

/// Response for update operation
#[derive(Debug, Serialize)]
#[cfg_attr(feature = "openapi", derive(utoipa::ToSchema))]
pub struct UpdateResponse {
    /// Whether the operation was successful
    pub success: bool,
    /// Message describing the result
    pub message: String,
}

/// Get current version information
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/version",
    tag = "system",
    responses(
        (status = 200, description = "Version information", body = VersionInfo)
    )
))]
pub async fn get_version(State(state): State<AppState>) -> Json<VersionInfo> {
    Json(state.update.get_version_info())
}

/// Check for updates (triggers auto-update if available)
#[cfg_attr(feature = "openapi", utoipa::path(
    post,
    path = "/api/version/check",
    tag = "system",
    responses(
        (status = 200, description = "Update check triggered", body = UpdateResponse)
    )
))]
pub async fn check_update(State(state): State<AppState>) -> AppResult<Json<UpdateResponse>> {
    state.update.check_for_updates().await.map_err(|e| {
        crate::error::AppError::internal(format!("Failed to check for updates: {}", e))
    })?;

    Ok(Json(UpdateResponse {
        success: true,
        message: "Update check triggered. If an update is available, it will be installed automatically.".to_string(),
    }))
}
