use axum::Json;
use serde::Deserialize;

use crate::error::AppResult;
use crate::services::{Downloader, DownloaderClient, DownloaderConfig, DownloaderType};

#[derive(Debug, Deserialize)]
pub struct TestDownloaderRequest {
    #[serde(rename = "type")]
    pub downloader_type: DownloaderType,
    pub url: String,
    pub username: String,
    pub password: String,
}

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
