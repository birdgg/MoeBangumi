use serde::{Deserialize, Serialize};
/// Download task status.
///
/// Normalized across different downloader implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskStatus {
    /// Task is queued and waiting to start
    Queued,

    /// Actively downloading
    Downloading,

    /// Download paused by user
    Paused,

    /// Download completed, now seeding (BitTorrent)
    Seeding,

    /// Download completed
    Completed,

    /// Download stalled (no peers/sources)
    Stalled,

    /// Checking files
    Checking,

    /// Error occurred
    Error,

    /// Unknown/other status
    Unknown,
}

impl TaskStatus {
    /// Check if task is actively transferring data
    pub fn is_active(&self) -> bool {
        matches!(self, Self::Downloading | Self::Seeding)
    }

    /// Check if task is finished downloading
    pub fn is_finished(&self) -> bool {
        matches!(self, Self::Seeding | Self::Completed)
    }
}

impl Default for TaskStatus {
    fn default() -> Self {
        Self::Unknown
    }
}
