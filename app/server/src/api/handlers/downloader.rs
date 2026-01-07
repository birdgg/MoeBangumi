use axum::Json;
use serde::Deserialize;

use crate::error::AppResult;
use crate::services::{Downloader, DownloaderClient, DownloaderConfig, DownloaderType};

/// Request body for testing downloader connection
#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "openapi", derive(utoipa::ToSchema))]
pub struct TestDownloaderRequest {
    /// Downloader type (e.g., "qbittorrent")
    #[serde(rename = "type")]
    pub downloader_type: DownloaderType,
    /// Downloader Web UI URL
    pub url: String,
    /// Username
    pub username: String,
    /// Password
    pub password: String,
}

/// Test downloader connection with provided credentials
#[cfg_attr(feature = "openapi", utoipa::path(
    post,
    path = "/api/downloader/test",
    tag = "downloader",
    request_body = TestDownloaderRequest,
    responses(
        (status = 200, description = "Connection successful"),
        (status = 401, description = "Authentication failed")
    )
))]
pub async fn test_downloader_connection(
    Json(payload): Json<TestDownloaderRequest>,
) -> AppResult<&'static str> {
    let config = DownloaderConfig {
        downloader_type: payload.downloader_type,
        url: payload.url,
        username: Some(payload.username),
        password: Some(payload.password),
    };

    let client = DownloaderClient::from_config(config)?;
    client.login().await?;

    Ok("Connection successful")
}
