use axum::{extract::State, http::StatusCode, response::IntoResponse};

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
pub async fn trigger_rss_fetch(State(state): State<AppState>) -> impl IntoResponse {
    tracing::info!("Manually triggering RSS fetch job");

    match state.rss_fetch_job.execute().await {
        Ok(()) => {
            tracing::info!("Manual RSS fetch job completed successfully");
            (StatusCode::OK, "RSS fetch job completed successfully").into_response()
        }
        Err(e) => {
            tracing::error!("Manual RSS fetch job failed: {}", e);
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("RSS fetch job failed: {}", e),
            )
                .into_response()
        }
    }
}
