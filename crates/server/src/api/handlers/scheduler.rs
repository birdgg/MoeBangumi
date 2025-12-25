use axum::extract::State;

use crate::error::{AppError, AppResult};
use crate::services::SchedulerJob;
use crate::state::AppState;

/// Manually trigger RSS fetch job
#[utoipa::path(
    post,
    path = "/api/scheduler/rss-fetch",
    tag = "scheduler",
    responses(
        (status = 200, description = "RSS fetch job triggered successfully"),
        (status = 500, description = "Job execution failed")
    )
)]
pub async fn trigger_rss_fetch(State(state): State<AppState>) -> AppResult<&'static str> {
    tracing::info!("Manually triggering RSS fetch job");

    state
        .rss_fetch_job
        .execute()
        .await
        .map_err(|e| AppError::internal(e.to_string()))?;

    tracing::info!("Manual RSS fetch job completed successfully");
    Ok("RSS fetch job completed successfully")
}
