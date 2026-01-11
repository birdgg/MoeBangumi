//! Scan API handlers

use axum::{extract::State, http::StatusCode, Json};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::infra::error::{AppError, AppResult};
use crate::state::AppState;

#[derive(Debug, Default, Deserialize)]
pub struct ScanImportRequest {}

#[derive(Debug, Serialize)]
pub struct ScanImportResponse {
    pub message: String,
    pub started_at: DateTime<Utc>,
}

pub async fn scan_import(
    State(state): State<AppState>,
    Json(_payload): Json<ScanImportRequest>,
) -> AppResult<(StatusCode, Json<ScanImportResponse>)> {
    let scan_service = Arc::clone(&state.services.scan);

    // Check if a scan is already in progress
    if !scan_service.try_start_scan() {
        return Err(AppError::conflict("A scan is already in progress"));
    }

    // Start background task
    tokio::spawn(async move {
        let result = scan_service.scan_and_import().await;

        tracing::info!(
            "Background scan completed: {} scanned, {} imported, {} skipped, {} failed",
            result.total_scanned,
            result.imported,
            result.skipped,
            result.failed
        );
    });

    Ok((
        StatusCode::ACCEPTED,
        Json(ScanImportResponse {
            message: "Scan started in background".to_string(),
            started_at: Utc::now(),
        }),
    ))
}
