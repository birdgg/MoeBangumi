use async_trait::async_trait;

use crate::error::{DownloaderError, Result};
use crate::models::{AddTorrentOptions, SyncMainData, TorrentFile, TorrentInfo};

/// Core downloader interface
///
/// This trait defines the common operations that all downloaders must support.
/// Extended features (like file renaming, autorun configuration) are in DownloaderExt.
#[async_trait]
pub trait Downloader: Send + Sync {
    /// Login to the downloader
    async fn login(&self) -> Result<()>;

    /// Check if currently logged in
    async fn is_login(&self) -> Result<bool>;

    /// Get all tasks
    async fn get_tasks(&self) -> Result<Vec<TorrentInfo>>;

    /// Get tasks with optional filtering
    ///
    /// # Arguments
    /// * `filter` - Optional torrent filter (e.g., "completed", "downloading")
    /// * `tag` - Optional tag filter
    async fn get_tasks_filtered(
        &self,
        filter: Option<&str>,
        tag: Option<&str>,
    ) -> Result<Vec<TorrentInfo>>;

    /// Get tasks info with incremental sync
    ///
    /// # Arguments
    /// * `rid` - Response ID from previous call. Use 0 for initial request.
    async fn get_tasks_info(&self, rid: i64) -> Result<SyncMainData>;

    /// Add a new task
    ///
    /// Returns an identifier for the added task (varies by implementation)
    async fn add_task(&self, options: AddTorrentOptions) -> Result<String>;

    /// Pause task(s) by hash
    ///
    /// # Arguments
    /// * `hashes` - List of task hashes. Use `&["all"]` to pause all tasks.
    async fn pause_task(&self, hashes: &[&str]) -> Result<()>;

    /// Resume task(s) by hash
    ///
    /// # Arguments
    /// * `hashes` - List of task hashes. Use `&["all"]` to resume all tasks.
    async fn resume_task(&self, hashes: &[&str]) -> Result<()>;

    /// Delete task(s) by hash
    ///
    /// # Arguments
    /// * `hashes` - List of task hashes
    /// * `delete_files` - Whether to delete downloaded files
    async fn delete_task(&self, hashes: &[&str], delete_files: bool) -> Result<()>;

    /// Add tags to a task
    ///
    /// # Arguments
    /// * `hash` - Task hash
    /// * `tags` - Tags to add
    async fn add_tags(&self, hash: &str, tags: &[&str]) -> Result<()>;

    /// Remove tags from a task
    ///
    /// # Arguments
    /// * `hash` - Task hash
    /// * `tags` - Tags to remove. If empty, all tags are removed.
    async fn remove_tags(&self, hash: &str, tags: &[&str]) -> Result<()>;

    /// Get files for a specific task
    async fn get_task_files(&self, hash: &str) -> Result<Vec<TorrentFile>>;

    /// Rename a file within a task
    async fn rename_file(&self, hash: &str, old_path: &str, new_path: &str) -> Result<()>;

    /// Get the downloader type name (for logging)
    fn downloader_type(&self) -> &'static str;
}

/// Extended downloader interface for advanced features
///
/// This trait provides optional functionality that may not be supported by all
/// downloader implementations. Default implementations return NotSupported errors.
#[async_trait]
pub trait DownloaderExt: Downloader {
    /// Get task information by hash
    async fn get_task_info(&self, _hash: &str) -> Result<Option<TorrentInfo>> {
        Err(DownloaderError::NotSupported("get_task_info".into()))
    }

    /// Configure autorun to call a webhook URL when tasks complete
    async fn configure_autorun(&self, _webhook_url: &str) -> Result<()> {
        Err(DownloaderError::NotSupported("configure_autorun".into()))
    }

    /// Disable autorun callback
    async fn disable_autorun(&self) -> Result<()> {
        Err(DownloaderError::NotSupported("disable_autorun".into()))
    }
}
