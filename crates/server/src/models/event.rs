use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;
use utoipa::{IntoParams, ToSchema};

/// Event severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize, ToSchema)]
#[serde(rename_all = "lowercase")]
pub enum EventLevel {
    #[default]
    Info,
    Warning,
    Error,
}

impl EventLevel {
    pub fn as_str(&self) -> &'static str {
        match self {
            EventLevel::Info => "info",
            EventLevel::Warning => "warning",
            EventLevel::Error => "error",
        }
    }
}

impl fmt::Display for EventLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for EventLevel {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "info" => Ok(EventLevel::Info),
            "warning" | "warn" => Ok(EventLevel::Warning),
            "error" => Ok(EventLevel::Error),
            _ => Err(format!("Invalid event level: {}", s)),
        }
    }
}

/// System event entity
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Event {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    /// 事件级别: info, warning, error
    pub level: EventLevel,
    /// 事件消息，简短描述发生了什么
    pub message: String,
    /// 详细信息，如错误堆栈或额外上下文
    pub details: Option<String>,
}

/// Request body for creating a new event
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateEvent {
    /// 事件级别: info, warning, error
    pub level: EventLevel,
    /// 事件消息，简短描述发生了什么
    pub message: String,
    /// 详细信息，如错误堆栈或额外上下文
    #[serde(default)]
    pub details: Option<String>,
}

/// Query parameters for event listing
#[derive(Debug, Default, Deserialize, IntoParams)]
pub struct EventQueryParams {
    /// Filter by event level
    pub level: Option<String>,
    /// Maximum number of events to return (default: 50, max: 500)
    pub limit: Option<i64>,
    /// Number of events to skip (for pagination)
    pub offset: Option<i64>,
}
