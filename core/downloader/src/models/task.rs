use serde::{Deserialize, Serialize};
use super::TaskStatus;

/// Unified download task representation.
///
/// This model represents a download task across different downloaders,
/// normalizing fields into a common structure.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    /// Unique identifier (hash for BitTorrent, ID for HTTP)
    pub id: String,

    /// Task name
    pub name: String,

    /// Current status
    pub status: TaskStatus,

    /// Download progress (0.0 to 1.0)
    pub progress: f64,

    /// Save path / download directory
    pub save_path: String,

    /// Total size in bytes
    pub total_size: i64,

    /// Downloaded bytes
    pub downloaded: i64,

    /// Estimated time to completion (seconds, -1 if unknown)
    pub eta: i64,

    /// Tags (comma-separated string for consistency)
    pub tags: String,

    /// Category (optional, some downloaders support this)
    #[serde(default)]
    pub category: Option<String>,
}

impl Task {
    /// Check if download is completed
    pub fn is_completed(&self) -> bool {
        matches!(self.status, TaskStatus::Seeding | TaskStatus::Completed) || self.progress >= 1.0
    }

    /// Get tags as a vector
    pub fn tags_vec(&self) -> Vec<String> {
        self.tags
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect()
    }

    /// Check if task has a specific tag
    pub fn has_tag(&self, tag: &str) -> bool {
        self.tags_vec().iter().any(|t| t == tag)
    }
}

impl Default for Task {
    fn default() -> Self {
        Self {
            id: String::new(),
            name: String::new(),
            status: TaskStatus::default(),
            progress: 0.0,
            save_path: String::new(),
            total_size: 0,
            downloaded: 0,
            eta: -1,
            tags: String::new(),
            category: None,
        }
    }
}
