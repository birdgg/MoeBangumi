use sqlx::SqlitePool;
use std::collections::VecDeque;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::{broadcast, RwLock};

use crate::models::{CreateLog, Log, LogLevel, LogQueryParams};
use crate::repositories::LogRepository;

#[derive(Debug, Error)]
pub enum LogError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Broadcast error")]
    Broadcast,
}

/// Buffer capacity for recent logs
const BUFFER_CAPACITY: usize = 100;
/// Broadcast channel capacity
const BROADCAST_CAPACITY: usize = 256;

/// Log service for recording and broadcasting system logs
pub struct LogService {
    db: SqlitePool,
    buffer: Arc<RwLock<LogBuffer>>,
    broadcaster: broadcast::Sender<Log>,
}

impl LogService {
    /// Create a new log service
    pub fn new(db: SqlitePool) -> Self {
        let (broadcaster, _) = broadcast::channel(BROADCAST_CAPACITY);
        Self {
            db,
            buffer: Arc::new(RwLock::new(LogBuffer::new(BUFFER_CAPACITY))),
            broadcaster,
        }
    }

    /// Record a log (saves to database and broadcasts to subscribers)
    pub async fn record(&self, data: CreateLog) -> Result<Log, LogError> {
        // Save to database
        let log = LogRepository::create(&self.db, data).await?;

        // Add to in-memory buffer
        self.buffer.write().await.push(log.clone());

        // Broadcast to SSE subscribers (ignore if no subscribers)
        let _ = self.broadcaster.send(log.clone());

        Ok(log)
    }

    /// Record a log without propagating errors (for use in background tasks)
    pub async fn record_safe(&self, data: CreateLog) {
        if let Err(e) = self.record(data).await {
            tracing::warn!("Failed to record log: {}", e);
        }
    }

    /// Record an info log
    pub async fn info(&self, message: impl Into<String>) {
        self.record_safe(CreateLog {
            level: LogLevel::Info,
            message: message.into(),
        })
        .await;
    }

    /// Record a warning log
    pub async fn warning(&self, message: impl Into<String>) {
        self.record_safe(CreateLog {
            level: LogLevel::Warning,
            message: message.into(),
        })
        .await;
    }

    /// Record an error log
    pub async fn error(&self, message: impl Into<String>) {
        self.record_safe(CreateLog {
            level: LogLevel::Error,
            message: message.into(),
        })
        .await;
    }

    /// Subscribe to log broadcasts (for SSE)
    pub fn subscribe(&self) -> broadcast::Receiver<Log> {
        self.broadcaster.subscribe()
    }

    /// Get recent logs from in-memory buffer
    pub async fn recent(&self, limit: usize) -> Vec<Log> {
        self.buffer.read().await.get_recent(limit)
    }

    /// List logs with optional filtering and pagination
    pub async fn list(&self, params: LogQueryParams) -> Result<Vec<Log>, LogError> {
        Ok(LogRepository::list(&self.db, params).await?)
    }

    /// Clean up old logs from database
    pub async fn cleanup(&self, days: i64) -> Result<u64, LogError> {
        let deleted = LogRepository::cleanup_old(&self.db, days).await?;
        if deleted > 0 {
            tracing::info!("Cleaned up {} old logs (older than {} days)", deleted, days);
        }
        Ok(deleted)
    }

    /// Clear all logs from database and buffer
    pub async fn clear_all(&self) -> Result<u64, LogError> {
        let deleted = LogRepository::delete_all(&self.db).await?;
        // Also clear the in-memory buffer
        self.buffer.write().await.clear();
        Ok(deleted)
    }
}

/// In-memory ring buffer for recent logs
struct LogBuffer {
    logs: VecDeque<Log>,
    capacity: usize,
}

impl LogBuffer {
    fn new(capacity: usize) -> Self {
        Self {
            logs: VecDeque::with_capacity(capacity),
            capacity,
        }
    }

    fn push(&mut self, log: Log) {
        if self.logs.len() >= self.capacity {
            self.logs.pop_front();
        }
        self.logs.push_back(log);
    }

    fn get_recent(&self, limit: usize) -> Vec<Log> {
        self.logs.iter().rev().take(limit).cloned().collect()
    }

    fn clear(&mut self) {
        self.logs.clear();
    }
}
