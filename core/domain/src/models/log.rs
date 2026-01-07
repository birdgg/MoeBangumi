use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;
#[cfg(feature = "openapi")]
use utoipa::{IntoParams, ToSchema};

/// Log severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
    #[default]
    Info,
    Warning,
    Error,
}

impl LogLevel {
    pub fn as_str(&self) -> &'static str {
        match self {
            LogLevel::Info => "info",
            LogLevel::Warning => "warning",
            LogLevel::Error => "error",
        }
    }
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for LogLevel {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "info" => Ok(LogLevel::Info),
            "warning" | "warn" => Ok(LogLevel::Warning),
            "error" => Ok(LogLevel::Error),
            _ => Err(format!("Invalid log level: {}", s)),
        }
    }
}

/// System log entity
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Log {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    /// 日志级别: info, warning, error
    pub level: LogLevel,
    /// 日志消息
    pub message: String,
}

/// Request body for creating a new log
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CreateLog {
    /// 日志级别: info, warning, error
    pub level: LogLevel,
    /// 日志消息
    pub message: String,
}

/// Query parameters for log listing
#[derive(Debug, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(IntoParams))]
pub struct LogQueryParams {
    /// Filter by log level
    pub level: Option<String>,
    /// Maximum number of logs to return (default: 50, max: 500)
    pub limit: Option<i64>,
    /// Number of logs to skip (for pagination)
    pub offset: Option<i64>,
}
