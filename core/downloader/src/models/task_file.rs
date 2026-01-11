use serde::{Deserialize, Serialize};
/// File within a download task.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskFile {
    /// File index within the task
    pub index: i32,

    /// File path (relative to task save_path)
    pub path: String,

    /// File size in bytes
    pub size: i64,

    /// Download progress (0.0 to 1.0)
    pub progress: f64,
}

impl TaskFile {
    /// Check if file is completed
    pub fn is_completed(&self) -> bool {
        self.progress >= 1.0
    }

    /// Check if this is a video file
    pub fn is_video(&self) -> bool {
        const VIDEO_EXTS: &[&str] = &["mp4", "mkv", "avi", "mov", "flv", "wmv", "webm", "m4v", "ts"];

        self.path
            .rsplit('.')
            .next()
            .map(|ext| VIDEO_EXTS.contains(&ext.to_lowercase().as_str()))
            .unwrap_or(false)
    }

    /// Get the file extension
    pub fn extension(&self) -> Option<&str> {
        self.path.rsplit('.').next()
    }
}
