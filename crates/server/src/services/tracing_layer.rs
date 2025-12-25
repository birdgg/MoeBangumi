use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::mpsc;
use tracing::field::{Field, Visit};
use tracing::{Event, Level, Subscriber};
use tracing_subscriber::layer::Context;
use tracing_subscriber::Layer;

use crate::models::EventLevel;
use crate::services::EventService;

/// Channel capacity for buffering log events
const CHANNEL_CAPACITY: usize = 1024;

/// Deduplication window - skip duplicate messages within this time period
const DEDUP_WINDOW: Duration = Duration::from_secs(60);

/// A log event captured from tracing
#[derive(Debug, Clone)]
pub struct LogEvent {
    pub level: EventLevel,
    pub message: String,
    pub details: Option<String>,
}

impl LogEvent {
    /// Generate a deduplication key for this event
    fn dedup_key(&self) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.level.hash(&mut hasher);
        self.message.hash(&mut hasher);
        hasher.finish()
    }
}

/// Sender half of the log channel
pub type LogSender = mpsc::Sender<LogEvent>;

/// Receiver half of the log channel
pub type LogReceiver = mpsc::Receiver<LogEvent>;

/// Create a new log channel pair
pub fn create_log_channel() -> (LogSender, LogReceiver) {
    mpsc::channel(CHANNEL_CAPACITY)
}

/// Tracing layer that captures error and warning logs
pub struct DatabaseLayer {
    sender: LogSender,
}

impl DatabaseLayer {
    /// Create a new database layer with the given sender
    pub fn new(sender: LogSender) -> Self {
        Self { sender }
    }
}

impl<S> Layer<S> for DatabaseLayer
where
    S: Subscriber,
{
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        let level = event.metadata().level();

        // Only capture error and warn levels
        let event_level = match *level {
            Level::INFO => EventLevel::Info,
            Level::ERROR => EventLevel::Error,
            Level::WARN => EventLevel::Warning,
            _ => return,
        };

        // Extract message and extra fields from the event
        let mut visitor = FieldVisitor::default();
        event.record(&mut visitor);

        let details = visitor.details();
        let log_event = LogEvent {
            level: event_level,
            message: visitor.message,
            details,
        };

        // Non-blocking send - drop if channel is full
        let _ = self.sender.try_send(log_event);
    }
}

/// Visitor to extract message and extra fields from tracing events
#[derive(Default)]
struct FieldVisitor {
    message: String,
    fields: Vec<(String, String)>,
}

impl FieldVisitor {
    /// Convert extra fields to JSON string for details
    fn details(&self) -> Option<String> {
        if self.fields.is_empty() {
            return None;
        }
        // Build a simple JSON object
        let pairs: Vec<String> = self
            .fields
            .iter()
            .map(|(k, v)| format!("\"{}\": \"{}\"", k, v.replace('\"', "\\\"")))
            .collect();
        Some(format!("{{{}}}", pairs.join(", ")))
    }
}

impl Visit for FieldVisitor {
    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        let name = field.name();
        let value_str = format!("{:?}", value);

        if name == "message" {
            self.message = value_str;
        } else {
            self.fields.push((name.to_string(), value_str));
        }
    }

    fn record_str(&mut self, field: &Field, value: &str) {
        let name = field.name();

        if name == "message" {
            self.message = value.to_string();
        } else {
            self.fields.push((name.to_string(), value.to_string()));
        }
    }

    fn record_i64(&mut self, field: &Field, value: i64) {
        self.fields
            .push((field.name().to_string(), value.to_string()));
    }

    fn record_u64(&mut self, field: &Field, value: u64) {
        self.fields
            .push((field.name().to_string(), value.to_string()));
    }

    fn record_bool(&mut self, field: &Field, value: bool) {
        self.fields
            .push((field.name().to_string(), value.to_string()));
    }

    fn record_error(&mut self, field: &Field, value: &(dyn std::error::Error + 'static)) {
        self.fields
            .push((field.name().to_string(), value.to_string()));
    }
}

/// Start the background task that writes log events to the database
pub fn start_log_writer(mut receiver: LogReceiver, events: Arc<EventService>) {
    tokio::spawn(async move {
        // Deduplication cache: key -> last seen time
        let mut seen: HashMap<u64, Instant> = HashMap::new();
        let mut last_cleanup = Instant::now();

        while let Some(log_event) = receiver.recv().await {
            let now = Instant::now();

            // Periodic cleanup of old entries (every 5 minutes)
            if now.duration_since(last_cleanup) > Duration::from_secs(300) {
                seen.retain(|_, &mut time| now.duration_since(time) < DEDUP_WINDOW);
                last_cleanup = now;
            }

            // Check for duplicate
            let key = log_event.dedup_key();
            if let Some(&last_seen) = seen.get(&key) {
                if now.duration_since(last_seen) < DEDUP_WINDOW {
                    // Skip duplicate within dedup window
                    continue;
                }
            }

            // Record this event
            seen.insert(key, now);

            match log_event.level {
                EventLevel::Error => {
                    events.error(&log_event.message, log_event.details).await;
                }
                EventLevel::Warning => {
                    events.warning(&log_event.message).await;
                }
                EventLevel::Info => {
                    events.info(&log_event.message).await;
                }
            }
        }
    });
}
