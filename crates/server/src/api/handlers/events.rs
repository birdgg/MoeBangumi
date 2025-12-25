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
use crate::models::{Event, EventQueryParams};
use crate::repositories::EventRepository;
use crate::state::AppState;

/// Get events with optional filtering and pagination
#[utoipa::path(
    get,
    path = "/api/events",
    tag = "events",
    params(EventQueryParams),
    responses(
        (status = 200, description = "Events retrieved successfully", body = Vec<Event>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_events(
    State(state): State<AppState>,
    Query(params): Query<EventQueryParams>,
) -> AppResult<Json<Vec<Event>>> {
    let events = EventRepository::list(&state.db, params).await?;
    Ok(Json(events))
}

/// Stream events via Server-Sent Events (SSE)
#[utoipa::path(
    get,
    path = "/api/events/stream",
    tag = "events",
    responses(
        (status = 200, description = "SSE event stream", content_type = "text/event-stream")
    )
)]
pub async fn stream_events(
    State(state): State<AppState>,
) -> Sse<impl Stream<Item = Result<SseEvent, Infallible>>> {
    // Get recent events for initial push
    let recent_events = state.events.recent(20).await;

    // Subscribe to new events
    let rx = state.events.subscribe();
    let broadcast_stream = BroadcastStream::new(rx);

    // Create stream that first sends recent events, then broadcasts new ones
    let initial_stream = futures::stream::iter(recent_events.into_iter().map(|event| {
        let data = serde_json::to_string(&event).unwrap_or_default();
        Ok(SseEvent::default().data(data))
    }));

    let live_stream = broadcast_stream.filter_map(|result| match result {
        Ok(event) => {
            let data = serde_json::to_string(&event).unwrap_or_default();
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

/// Delete old events (cleanup endpoint)
#[utoipa::path(
    delete,
    path = "/api/events",
    tag = "events",
    responses(
        (status = 200, description = "Old events deleted", body = u64),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn cleanup_events(State(state): State<AppState>) -> AppResult<Json<u64>> {
    // Delete events older than 30 days
    let deleted = state.events.cleanup(30).await?;
    Ok(Json(deleted))
}
