use axum::{extract::State, Json};
use reqwest::{Client, Proxy};
use serde::Deserialize;

use crate::error::AppResult;
use crate::models::{Settings, UpdateSettings};
use crate::state::AppState;

/// Request body for testing proxy connection
#[derive(Debug, Deserialize, utoipa::ToSchema)]
pub struct TestProxyRequest {
    /// Proxy server URL (e.g., http://127.0.0.1:7890)
    pub url: String,
    /// Proxy username (optional)
    #[serde(default)]
    pub username: Option<String>,
    /// Proxy password (optional)
    #[serde(default)]
    pub password: Option<String>,
}

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

    // Configure qBittorrent autorun if webhook_url is set
    if !settings.downloader.webhook_url.is_empty() {
        if let Err(e) = state
            .downloader
            .configure_autorun(&settings.downloader.webhook_url)
            .await
        {
            tracing::warn!("Failed to configure downloader autorun: {}", e);
        }
    }

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

/// Test proxy connection by making a request to mikanani.me
#[utoipa::path(
    post,
    path = "/api/proxy/test",
    tag = "settings",
    request_body = TestProxyRequest,
    responses(
        (status = 200, description = "Proxy connection successful"),
        (status = 400, description = "Invalid proxy configuration"),
        (status = 500, description = "Proxy connection failed")
    )
)]
pub async fn test_proxy(Json(payload): Json<TestProxyRequest>) -> AppResult<&'static str> {
    // Build proxy with optional authentication
    let mut proxy = Proxy::all(&payload.url)
        .map_err(|e| crate::error::AppError::BadRequest(format!("Invalid proxy URL: {}", e)))?;

    if let Some(username) = &payload.username {
        if !username.is_empty() {
            let password = payload.password.as_deref().unwrap_or("");
            proxy = proxy.basic_auth(username, password);
        }
    }

    // Create a temporary client with the proxy
    let client = Client::builder()
        .proxy(proxy)
        .timeout(std::time::Duration::from_secs(10))
        .build()
        .map_err(|e| crate::error::AppError::BadRequest(format!("Failed to build client: {}", e)))?;

    // Test by making a request to mikanani.me
    let response = client
        .get("https://mikanani.me/")
        .send()
        .await
        .map_err(|e| crate::error::AppError::Internal(format!("Proxy connection failed: {}", e)))?;

    if response.status().is_success() {
        tracing::info!("Proxy connection test successful");
        Ok("Proxy connection successful")
    } else {
        Err(crate::error::AppError::Internal(format!(
            "Proxy test failed with status: {}",
            response.status()
        )))
    }
}
