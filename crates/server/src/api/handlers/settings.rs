use axum::{extract::State, Json};

use crate::error::AppResult;
use crate::models::{Settings, UpdateSettings};
use crate::state::AppState;

/// Get application settings
#[utoipa::path(
    get,
    path = "/api/settings",
    tag = "settings",
    responses(
        (status = 200, description = "Application settings", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_settings(State(state): State<AppState>) -> Json<Settings> {
    Json(state.settings.get())
}

/// Update application settings
#[utoipa::path(
    patch,
    path = "/api/settings",
    tag = "settings",
    request_body = UpdateSettings,
    responses(
        (status = 200, description = "Settings updated successfully", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn update_settings(
    State(state): State<AppState>,
    Json(payload): Json<UpdateSettings>,
) -> AppResult<Json<Settings>> {
    let settings = state.settings.update(payload).await?;
    Ok(Json(settings))
}

/// Reset settings to defaults
#[utoipa::path(
    post,
    path = "/api/settings/reset",
    tag = "settings",
    responses(
        (status = 200, description = "Settings reset successfully", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn reset_settings(State(state): State<AppState>) -> AppResult<Json<Settings>> {
    let settings = state.settings.reset().await?;
    Ok(Json(settings))
}
