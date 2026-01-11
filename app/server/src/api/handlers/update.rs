//! Update API handlers

use axum::{extract::State, Json};
use serde::Serialize;
use updater::VersionInfo;

use crate::error::AppResult;
use crate::state::AppState;

#[derive(Debug, Serialize)]
pub struct UpdateResponse {
    pub success: bool,
    pub message: String,
}

pub async fn get_version(State(state): State<AppState>) -> Json<VersionInfo> {
    Json(state.services.update.get_version_info())
}

pub async fn check_update(State(state): State<AppState>) -> AppResult<Json<VersionInfo>> {
    state.services.update.check_only().await.map_err(|e| {
        crate::error::AppError::internal(format!("Failed to check for updates: {}", e))
    })?;

    // Wait a moment for the check to complete
    tokio::time::sleep(std::time::Duration::from_millis(500)).await;

    Ok(Json(state.services.update.get_version_info()))
}

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
