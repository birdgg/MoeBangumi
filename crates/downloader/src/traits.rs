use async_trait::async_trait;

use crate::error::Result;
use crate::models::{AddTaskOptions, Task, TaskFile, TaskFilter};

/// Core downloader interface.
///
/// This trait defines the essential operations that all downloaders must support.
/// Implementations should convert their native types to the unified models defined
/// in this crate (Task, TaskFile, TaskStatus).
///
/// # Design Principles
///
/// - **Simplicity**: Only 8 core methods, no bloat
/// - **Universality**: All methods work across BitTorrent, HTTP, and other protocols
/// - **Type safety**: Strongly typed models instead of stringly-typed APIs
/// - **Async first**: All operations are async for proper I/O handling
///
/// # Thread Safety
///
/// All implementations must be Send + Sync for use in async contexts.
#[async_trait]
pub trait Downloader: Send + Sync {
    /// Authenticate with the downloader.
    ///
    /// Some downloaders require authentication before operations can be performed.
    /// This method should be called before any other operations.
    ///
    /// # Errors
    ///
    /// Returns `DownloaderError::Auth` if authentication fails.
    async fn login(&self) -> Result<()>;

    /// Check if currently authenticated.
    ///
    /// # Returns
    ///
    /// - `Ok(true)` if authenticated and ready
    /// - `Ok(false)` if not authenticated
    /// - `Err(_)` if unable to check authentication status
    async fn is_login(&self) -> Result<bool>;

    /// Add a new download task.
    ///
    /// # Arguments
    ///
    /// * `options` - Configuration for the new task (URL, save path, tags, etc.)
    ///
    /// # Returns
    ///
    /// The task ID on success. Format varies by implementation:
    /// - BitTorrent: info hash
    /// - HTTP: generated UUID or sequential ID
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let options = AddTaskOptions::new("magnet:?xt=...")
    ///     .save_path("/downloads")
    ///     .add_tag("anime")
    ///     .category("tv");
    /// let task_id = downloader.add_task(options).await?;
    /// ```
    async fn add_task(&self, options: AddTaskOptions) -> Result<String>;

    /// Delete task(s) by ID.
    ///
    /// # Arguments
    ///
    /// * `ids` - List of task IDs to delete
    /// * `delete_files` - Whether to also delete downloaded files from disk
    ///
    /// # Notes
    ///
    /// - If a task ID doesn't exist, it's silently ignored
    /// - All specified tasks are deleted in a single operation
    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<()>;

    /// Get tasks with optional filtering.
    ///
    /// # Arguments
    ///
    /// * `filter` - Optional filter criteria (status, category, tag, IDs)
    ///
    /// # Returns
    ///
    /// Vector of tasks matching the filter. Returns all tasks if filter is None.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// // Get all tasks
    /// let all = downloader.get_tasks(None).await?;
    ///
    /// // Get completed tasks with "rename" tag
    /// let filter = TaskFilter::new()
    ///     .status(TaskStatus::Completed)
    ///     .tag("rename");
    /// let completed = downloader.get_tasks(Some(&filter)).await?;
    ///
    /// // Get specific task by ID
    /// let task = downloader.get_tasks(Some(&TaskFilter::new().id("abc123"))).await?;
    /// ```
    async fn get_tasks(&self, filter: Option<&TaskFilter>) -> Result<Vec<Task>>;

    /// Get files within a specific task.
    ///
    /// # Arguments
    ///
    /// * `id` - Task ID
    ///
    /// # Returns
    ///
    /// Vector of files in the task. For single-file tasks, returns one element.
    ///
    /// # Errors
    ///
    /// Returns error if task doesn't exist.
    async fn get_task_files(&self, id: &str) -> Result<Vec<TaskFile>>;

    /// Add tags to a task.
    ///
    /// # Arguments
    ///
    /// * `id` - Task ID
    /// * `tags` - Tags to add
    ///
    /// # Notes
    ///
    /// - If tag already exists, it's not duplicated
    /// - If task doesn't exist, returns error
    async fn add_tags(&self, id: &str, tags: &[&str]) -> Result<()>;

    /// Remove tags from a task.
    ///
    /// # Arguments
    ///
    /// * `id` - Task ID
    /// * `tags` - Tags to remove. If empty, all tags are removed.
    ///
    /// # Notes
    ///
    /// - If tag doesn't exist, it's silently ignored
    /// - If task doesn't exist, returns error
    async fn remove_tags(&self, id: &str, tags: &[&str]) -> Result<()>;

    /// Rename a file within a task.
    ///
    /// # Arguments
    ///
    /// * `id` - Task ID (e.g., torrent hash)
    /// * `old_path` - Old file path (relative to task content root)
    /// * `new_path` - New file path (relative to task content root)
    ///
    /// # Notes
    ///
    /// - Only the filename is changed, not the extension
    /// - Path must be relative to the task's content root
    /// - If task doesn't exist, returns error
    async fn rename_file(&self, id: &str, old_path: &str, new_path: &str) -> Result<()>;
}
