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
    Json(state.services.update.get_version_info())
}

/// Check for updates (check only, does not auto-update)
#[cfg_attr(feature = "openapi", utoipa::path(
    post,
    path = "/api/version/check",
    tag = "system",
    responses(
        (status = 200, description = "Version information after check", body = VersionInfo)
    )
))]
pub async fn check_update(State(state): State<AppState>) -> AppResult<Json<VersionInfo>> {
    state.services.update.check_only().await.map_err(|e| {
        crate::error::AppError::internal(format!("Failed to check for updates: {}", e))
    })?;

    // Wait a moment for the check to complete
    tokio::time::sleep(std::time::Duration::from_millis(500)).await;

    Ok(Json(state.services.update.get_version_info()))
}

/// Perform update (download and install)
#[cfg_attr(feature = "openapi", utoipa::path(
    post,
    path = "/api/version/update",
    tag = "system",
    responses(
        (status = 200, description = "Update triggered", body = UpdateResponse)
    )
))]
pub async fn perform_update(State(state): State<AppState>) -> AppResult<Json<UpdateResponse>> {
    // Check if update is available
    let version_info = state.services.update.get_version_info();
    if !version_info.update_available {
        return Ok(Json(UpdateResponse {
            success: false,
            message: "No update available".to_string(),
        }));
    }

    state.services.update.perform_update().await.map_err(|e| {
        crate::error::AppError::internal(format!("Failed to trigger update: {}", e))
    })?;

    Ok(Json(UpdateResponse {
        success: true,
        message: "Update started. The application will restart shortly.".to_string(),
    }))
}
