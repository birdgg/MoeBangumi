use sqlx::SqlitePool;
use std::collections::VecDeque;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::{broadcast, RwLock};

use crate::models::{CreateEvent, Event, EventLevel};
use crate::repositories::EventRepository;

#[derive(Debug, Error)]
pub enum EventError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Broadcast error")]
    Broadcast,
}

/// Buffer capacity for recent events
const BUFFER_CAPACITY: usize = 100;
/// Broadcast channel capacity
const BROADCAST_CAPACITY: usize = 256;

/// Event service for recording and broadcasting system events
pub struct EventService {
    db: SqlitePool,
    buffer: Arc<RwLock<EventBuffer>>,
    broadcaster: broadcast::Sender<Event>,
}

impl EventService {
    /// Create a new event service
    pub fn new(db: SqlitePool) -> Self {
        let (broadcaster, _) = broadcast::channel(BROADCAST_CAPACITY);
        Self {
            db,
            buffer: Arc::new(RwLock::new(EventBuffer::new(BUFFER_CAPACITY))),
            broadcaster,
        }
    }

    /// Record an event (saves to database and broadcasts to subscribers)
    pub async fn record(&self, data: CreateEvent) -> Result<Event, EventError> {
        // Save to database
        let event = EventRepository::create(&self.db, data).await?;

        // Add to in-memory buffer
        self.buffer.write().await.push(event.clone());

        // Broadcast to SSE subscribers (ignore if no subscribers)
        let _ = self.broadcaster.send(event.clone());

        Ok(event)
    }

    /// Record an event without propagating errors (for use in background tasks)
    pub async fn record_safe(&self, data: CreateEvent) {
        if let Err(e) = self.record(data).await {
            tracing::warn!("Failed to record event: {}", e);
        }
    }

    /// Record an info event
    pub async fn info(&self, message: impl Into<String>) {
        self.record_safe(CreateEvent {
            level: EventLevel::Info,
            message: message.into(),
            details: None,
        })
        .await;
    }

    /// Record a warning event
    pub async fn warning(&self, message: impl Into<String>) {
        self.record_safe(CreateEvent {
            level: EventLevel::Warning,
            message: message.into(),
            details: None,
        })
        .await;
    }

    /// Record an error event
    pub async fn error(&self, message: impl Into<String>, details: Option<String>) {
        self.record_safe(CreateEvent {
            level: EventLevel::Error,
            message: message.into(),
            details,
        })
        .await;
    }

    /// Subscribe to event broadcasts (for SSE)
    pub fn subscribe(&self) -> broadcast::Receiver<Event> {
        self.broadcaster.subscribe()
    }

    /// Get recent events from in-memory buffer
    pub async fn recent(&self, limit: usize) -> Vec<Event> {
        self.buffer.read().await.get_recent(limit)
    }

    /// Clean up old events from database
    pub async fn cleanup(&self, days: i64) -> Result<u64, EventError> {
        let deleted = EventRepository::cleanup_old(&self.db, days).await?;
        if deleted > 0 {
            tracing::info!("Cleaned up {} old events (older than {} days)", deleted, days);
        }
        Ok(deleted)
    }
}

/// In-memory ring buffer for recent events
struct EventBuffer {
    events: VecDeque<Event>,
    capacity: usize,
}

impl EventBuffer {
    fn new(capacity: usize) -> Self {
        Self {
            events: VecDeque::with_capacity(capacity),
            capacity,
        }
    }

    fn push(&mut self, event: Event) {
        if self.events.len() >= self.capacity {
            self.events.pop_front();
        }
        self.events.push_back(event);
    }

    fn get_recent(&self, limit: usize) -> Vec<Event> {
        self.events.iter().rev().take(limit).cloned().collect()
    }
}
