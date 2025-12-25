use axum::{
    extract::{Query, State},
    response::sse::{Event as SseEvent, KeepAlive},
    response::Sse,
    Json,
};
use futures::stream::Stream;
use std::{convert::Infallible, time::Duration};
use tokio_stream::wrappers::BroadcastStream;
use tokio_stream::StreamExt;

use crate::error::AppResult;
use crate::models::{Log, LogQueryParams};
use crate::state::AppState;

/// Get logs with optional filtering and pagination
#[utoipa::path(
    get,
    path = "/api/logs",
    tag = "logs",
    params(LogQueryParams),
    responses(
        (status = 200, description = "Logs retrieved successfully", body = Vec<Log>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_logs(
    State(state): State<AppState>,
    Query(params): Query<LogQueryParams>,
) -> AppResult<Json<Vec<Log>>> {
    let logs = state.logs.list(params).await?;
    Ok(Json(logs))
}

/// Stream logs via Server-Sent Events (SSE)
#[utoipa::path(
    get,
    path = "/api/logs/stream",
    tag = "logs",
    responses(
        (status = 200, description = "SSE log stream", content_type = "text/event-stream")
    )
)]
pub async fn stream_logs(
    State(state): State<AppState>,
) -> Sse<impl Stream<Item = Result<SseEvent, Infallible>>> {
    // Get recent logs for initial push
    let recent_logs = state.logs.recent(20).await;

    // Subscribe to new logs
    let rx = state.logs.subscribe();
    let broadcast_stream = BroadcastStream::new(rx);

    // Create stream that first sends recent logs, then broadcasts new ones
    let initial_stream = futures::stream::iter(recent_logs.into_iter().map(|log| {
        let data = serde_json::to_string(&log).unwrap_or_default();
        Ok(SseEvent::default().data(data))
    }));

    let live_stream = broadcast_stream.filter_map(|result| match result {
        Ok(log) => {
            let data = serde_json::to_string(&log).unwrap_or_default();
            Some(Ok(SseEvent::default().data(data)))
        }
        Err(_) => None, // Skip lagged messages
    });

    let combined_stream = initial_stream.chain(live_stream);

    Sse::new(combined_stream).keep_alive(
        KeepAlive::new()
            .interval(Duration::from_secs(15))
            .text("keep-alive"),
    )
}

/// Delete old logs (cleanup endpoint)
#[utoipa::path(
    delete,
    path = "/api/logs",
    tag = "logs",
    responses(
        (status = 200, description = "Old logs deleted", body = u64),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn cleanup_logs(State(state): State<AppState>) -> AppResult<Json<u64>> {
    // Delete logs older than 30 days
    let deleted = state.logs.cleanup(30).await?;
    Ok(Json(deleted))
}
