//! File renaming service for media organization
//!
//! This service handles automatic renaming of downloaded media files
//! to be compatible with Plex/Jellyfin naming standards.
//!
//! # Module Structure
//!
//! - `standard` - Standard WebRip task processing
//! - `bdrip` - BDRip (Blu-ray) task processing with complex directory structures
//! - `untracked` - Processing for torrents not tracked in database
//! - `utils` - Shared utility functions (path generation, subtitle handling)

use futures::stream::{self, StreamExt};
use parser::Parser;
use sqlx::SqlitePool;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use crate::config::Config;
use crate::models::{Bangumi, Torrent};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::torrent_metadata_resolver::TorrentMetadataResolver;
use crate::services::{DownloaderHandle, NotificationService, Task};

// Sub-modules
mod bdrip;
mod standard;
mod untracked;
mod utils;

use bdrip::BDRipProcessor;
use standard::StandardProcessor;
use untracked::UntrackedProcessor;

/// Error type for rename operations
#[derive(Debug, thiserror::Error)]
pub enum RenameError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Downloader error: {0}")]
    Downloader(#[from] downloader::DownloaderError),

    #[error("Torrent not found in database: {0}")]
    TorrentNotFound(String),

    #[error("Bangumi not found for torrent: {0}")]
    BangumiNotFound(i64),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, RenameError>;

/// Pending task with optional tracking info
///
/// For tracked torrents: has Torrent and Bangumi
/// For untracked torrents: has only Bangumi (created via resolver)
enum PendingTask {
    /// Tracked torrent with full context
    Tracked {
        task: Task,
        torrent: Torrent,
        bangumi: Bangumi,
    },
    /// Untracked torrent with resolved metadata
    Untracked {
        task: Task,
        bangumi: Bangumi,
    },
    /// Untracked torrent with fallback (no metadata found)
    Fallback {
        task: Task,
        title: String,
        season: Option<i32>,
    },
}

/// Result of a rename task processing
#[derive(Debug)]
pub struct RenameTaskResult {
    /// Bangumi ID
    pub bangumi_id: i64,
    /// Bangumi title (Chinese)
    pub bangumi_title: String,
    /// Poster URL (local path like /posters/xxx.jpg)
    pub poster_url: Option<String>,
    /// Total episodes for this bangumi (0 = unknown)
    pub total_episodes: i32,
    /// Successfully renamed episode numbers
    pub renamed_episodes: Vec<i32>,
}

/// Service for renaming downloaded media files to Plex/Jellyfin compatible names.
///
/// The service:
/// 1. Queries completed tasks pending rename from downloader
/// 2. Matches them with Torrent records in the database
/// 3. Gets Bangumi metadata for proper naming
/// 4. Renames video files and associated subtitles
/// 5. Marks tasks as rename complete
///
/// For untracked torrents (not in database), the service can optionally:
/// - Search metadata from BGM.tv and TMDB
/// - Create new Bangumi records
/// - Rename files using the found metadata
pub struct RenameService {
    db: SqlitePool,
    downloader: Arc<DownloaderHandle>,
    notification: Arc<NotificationService>,
    config: Arc<Config>,
    parser: Parser,
    /// Maximum number of concurrent rename tasks
    concurrency: usize,
    /// Optional resolver for untracked torrents
    resolver: Option<Arc<TorrentMetadataResolver>>,
}

impl RenameService {
    /// Create a new RenameService with default concurrency of 4
    pub fn new(
        db: SqlitePool,
        downloader: Arc<DownloaderHandle>,
        notification: Arc<NotificationService>,
        config: Arc<Config>,
    ) -> Self {
        Self {
            db,
            downloader,
            notification,
            config,
            parser: Parser::new(),
            concurrency: 4,
            resolver: None,
        }
    }

    /// Set the maximum number of concurrent rename tasks
    pub fn with_concurrency(mut self, concurrency: usize) -> Self {
        self.concurrency = concurrency;
        self
    }

    /// Set the metadata resolver for handling untracked torrents
    pub fn with_resolver(mut self, resolver: Arc<TorrentMetadataResolver>) -> Self {
        self.resolver = Some(resolver);
        self
    }

    /// Process all pending rename tasks
    ///
    /// This is the main entry point called by the scheduler.
    pub async fn process_all(&self) -> Result<()> {
        let pending = self.get_pending_tasks().await?;

        if pending.is_empty() {
            tracing::debug!("No tasks pending rename");
            return Ok(());
        }

        tracing::info!("Found {} tasks to rename", pending.len());

        // Collect results from all tasks
        let results: Vec<RenameTaskResult> = stream::iter(pending)
            .map(|pending_task| async move {
                match pending_task {
                    PendingTask::Tracked {
                        task,
                        torrent,
                        bangumi,
                    } => match self.process_tracked_task(&task, &torrent, &bangumi).await {
                        Ok(result) => Some(result),
                        Err(e) => {
                            tracing::error!(
                                "Failed to rename tracked task '{}' ({}): {}",
                                task.name,
                                task.id,
                                e
                            );
                            None
                        }
                    },
                    PendingTask::Untracked { task, bangumi } => {
                        match UntrackedProcessor::process(
                            &self.downloader,
                            &self.parser,
                            &task,
                            &bangumi,
                        )
                        .await
                        {
                            Ok(result) => {
                                // Finalize task after processing
                                if let Err(e) = self.finalize_task(&task.id).await {
                                    tracing::error!("Failed to finalize task {}: {}", task.id, e);
                                }
                                Some(result)
                            }
                            Err(e) => {
                                tracing::error!(
                                    "Failed to rename untracked task '{}' ({}): {}",
                                    task.name,
                                    task.id,
                                    e
                                );
                                None
                            }
                        }
                    }
                    PendingTask::Fallback {
                        task,
                        title,
                        season,
                    } => {
                        match UntrackedProcessor::process_fallback(
                            &self.downloader,
                            &self.parser,
                            &task,
                            &title,
                            season,
                        )
                        .await
                        {
                            Ok(result) => {
                                // Finalize task after processing
                                if let Err(e) = self.finalize_task(&task.id).await {
                                    tracing::error!("Failed to finalize task {}: {}", task.id, e);
                                }
                                Some(result)
                            }
                            Err(e) => {
                                tracing::error!(
                                    "Failed to rename fallback task '{}' ({}): {}",
                                    task.name,
                                    task.id,
                                    e
                                );
                                None
                            }
                        }
                    }
                }
            })
            .buffer_unordered(self.concurrency)
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect();

        // Group results by bangumi_id: (title, poster_url, total_episodes, episodes)
        let mut grouped: HashMap<i64, (String, Option<String>, i32, Vec<i32>)> = HashMap::new();
        for result in results {
            if result.renamed_episodes.is_empty() {
                continue;
            }
            let entry = grouped.entry(result.bangumi_id).or_insert_with(|| {
                (
                    result.bangumi_title.clone(),
                    result.poster_url.clone(),
                    result.total_episodes,
                    Vec::new(),
                )
            });
            entry.3.extend(result.renamed_episodes);
        }

        // Process each bangumi: update current_episode and send notification
        for (bangumi_id, (title, poster_url, total_episodes, mut episodes)) in grouped {
            if episodes.is_empty() {
                continue;
            }

            // Sort and deduplicate first, then get max from sorted list
            episodes.sort_unstable();
            episodes.dedup();

            let Some(&max_episode) = episodes.last() else {
                continue;
            };

            // Update current_episode (only if greater than current value)
            if let Err(e) = BangumiRepository::update_current_episode_if_greater(
                &self.db,
                bangumi_id,
                max_episode,
            )
            .await
            {
                tracing::warn!(
                    "Failed to update current_episode for bangumi {}: {}",
                    bangumi_id,
                    e
                );
            }

            // Auto-disable RSS when all episodes are downloaded
            // Only check if total_episodes is known (> 0)
            if total_episodes > 0 && max_episode >= total_episodes {
                match RssRepository::disable_by_bangumi_id(&self.db, bangumi_id).await {
                    Ok(count) if count > 0 => {
                        tracing::info!(
                            "Auto-disabled {} RSS subscription(s) for completed bangumi '{}' (episode {}/{})",
                            count,
                            title,
                            max_episode,
                            total_episodes
                        );
                    }
                    Err(e) => {
                        tracing::warn!(
                            "Failed to auto-disable RSS for bangumi {}: {}",
                            bangumi_id,
                            e
                        );
                    }
                    _ => {}
                }
            }

            // Send notification with poster if available
            let episode_str = Self::format_episode_range(&episodes);
            let notification_title = format!("{} 第{}话", title, episode_str);
            let notification_content = "已更新";

            // Send notification (fire-and-forget)
            if let Some(ref poster_path) = poster_url {
                self.send_notification_with_poster(
                    &notification_title,
                    notification_content,
                    poster_path,
                )
                .await;
            } else {
                self.notification
                    .notify_download(&notification_title, notification_content);
            }
        }

        Ok(())
    }

    /// Format episode numbers into a readable range string
    ///
    /// Examples:
    /// - [1] -> "01"
    /// - [1, 2, 3] -> "01-03"
    /// - [1, 3, 5] -> "01, 03, 05"
    /// - [1, 2, 3, 5, 6] -> "01-03, 05-06"
    fn format_episode_range(episodes: &[i32]) -> String {
        if episodes.is_empty() {
            return String::new();
        }
        if episodes.len() == 1 {
            return format!("{:02}", episodes[0]);
        }

        let mut ranges: Vec<(i32, i32)> = Vec::new();
        let mut start = episodes[0];
        let mut end = episodes[0];

        for &ep in &episodes[1..] {
            if ep == end + 1 {
                end = ep;
            } else {
                ranges.push((start, end));
                start = ep;
                end = ep;
            }
        }
        ranges.push((start, end));

        ranges
            .into_iter()
            .map(|(s, e)| {
                if s == e {
                    format!("{:02}", s)
                } else {
                    format!("{:02}-{:02}", s, e)
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Get all completed tasks pending rename
    async fn get_pending_tasks(&self) -> Result<Vec<PendingTask>> {
        // Query downloader for completed/seeding tasks with rename tag
        let all_tasks = self.downloader.get_rename_pending_tasks().await?;

        let mut result = Vec::new();

        for task in all_tasks {
            // Find matching torrent in database by info_hash
            if let Some(torrent) = TorrentRepository::get_by_info_hash(&self.db, &task.id).await? {
                // Get associated bangumi (only if bangumi_id is set)
                if let Some(bangumi_id) = torrent.bangumi_id {
                    if let Some(bangumi) =
                        BangumiRepository::get_by_id(&self.db, bangumi_id).await?
                    {
                        result.push(PendingTask::Tracked {
                            task,
                            torrent,
                            bangumi,
                        });
                    } else {
                        tracing::warn!(
                            "Bangumi not found for torrent {} (bangumi_id: {})",
                            task.id,
                            bangumi_id
                        );
                    }
                } else {
                    tracing::debug!(
                        "Torrent {} has no bangumi_id, treating as untracked",
                        task.id
                    );
                }
            } else {
                // Untracked torrent - try to resolve via metadata search
                if let Some(resolver) = &self.resolver {
                    match resolver.resolve(&task.name).await {
                        Ok(resolved) => {
                            if resolved.is_new {
                                tracing::info!(
                                    "Auto-created bangumi '{}' for untracked torrent '{}'",
                                    resolved.bangumi.title_chinese,
                                    task.name
                                );
                            } else {
                                tracing::info!(
                                    "Using existing bangumi '{}' for untracked torrent '{}'",
                                    resolved.bangumi.title_chinese,
                                    task.name
                                );
                            }
                            result.push(PendingTask::Untracked {
                                task,
                                bangumi: resolved.bangumi,
                            });
                        }
                        Err(e) => {
                            tracing::warn!(
                                "Failed to resolve metadata for '{}': {}, trying fallback",
                                task.name,
                                e
                            );
                            // Try fallback rename using parsed title
                            match resolver.resolve_fallback(&task.name).await {
                                Ok((search_info, _metadata)) => {
                                    if let Some(title) = search_info.best_search_title() {
                                        result.push(PendingTask::Fallback {
                                            task,
                                            title: title.to_string(),
                                            season: search_info.season,
                                        });
                                    } else {
                                        tracing::warn!(
                                            "No title extracted from task '{}', skipping",
                                            task.name
                                        );
                                    }
                                }
                                Err(e) => {
                                    tracing::warn!(
                                        "Fallback resolution failed for '{}': {}, skipping",
                                        task.name,
                                        e
                                    );
                                }
                            }
                        }
                    }
                } else {
                    tracing::debug!(
                        "Task '{}' ({}) not found in database and no resolver configured, skipping",
                        task.name,
                        task.id
                    );
                }
            }
        }

        Ok(result)
    }

    /// Process a tracked task (torrent exists in database)
    ///
    /// Returns the result containing bangumi info and successfully renamed episodes.
    async fn process_tracked_task(
        &self,
        task: &Task,
        torrent: &Torrent,
        bangumi: &Bangumi,
    ) -> Result<RenameTaskResult> {
        tracing::info!(
            "Processing task: {} ({}) for bangumi: {}",
            task.name,
            task.id,
            bangumi.title_chinese
        );

        // Check if task is in temporary directory and move to final location first
        use crate::utils::is_temp_download_path;
        if is_temp_download_path(&task.save_path) {
            let final_path = &bangumi.save_path;
            tracing::info!(
                "Moving task from temporary location {} to final location {}",
                task.save_path,
                final_path
            );

            match self.downloader.set_location(&task.id, final_path).await {
                Ok(()) => {
                    tracing::info!("Successfully moved task {} to {}", task.id, final_path);
                }
                Err(e) => {
                    tracing::error!(
                        "Failed to move task {} to final location: {}. Will retry on next rename cycle.",
                        task.id,
                        e
                    );
                    // Return error to skip this task - it will be retried next cycle
                    // since the rename tag hasn't been removed
                    return Err(RenameError::Downloader(e));
                }
            }
        }

        // Get file list from downloader
        let files = self.downloader.get_task_files(&task.id).await?;

        // Filter to video files only
        let video_files: Vec<_> = files.iter().filter(|f| f.is_video()).collect();

        if video_files.is_empty() {
            tracing::warn!("No video files found in task: {}", task.name);
            // Still remove the tag since there's nothing to rename
            self.finalize_task(&task.id).await?;
            return Ok(RenameTaskResult {
                bangumi_id: bangumi.id,
                bangumi_title: bangumi.title_chinese.clone(),
                poster_url: bangumi.poster_url.clone(),
                total_episodes: bangumi.total_episodes,
                renamed_episodes: Vec::new(),
            });
        }

        // Check if this is a BDRip task
        let is_bdrip = BDRipProcessor::is_bdrip(task, bangumi, &files);

        let renamed_episodes = if is_bdrip {
            tracing::info!("Detected BDRip structure for task: {}", task.name);
            BDRipProcessor::process(&self.downloader, task, &video_files, bangumi, &files).await?
        } else {
            StandardProcessor::process(
                &self.downloader,
                &self.parser,
                task,
                &video_files,
                torrent,
                bangumi,
                &files,
            )
            .await?
        };

        // Remove the "rename" tag
        self.finalize_task(&task.id).await?;

        tracing::info!("Successfully renamed task: {}", task.name);
        Ok(RenameTaskResult {
            bangumi_id: bangumi.id,
            bangumi_title: bangumi.title_chinese.clone(),
            poster_url: bangumi.poster_url.clone(),
            total_episodes: bangumi.total_episodes,
            renamed_episodes,
        })
    }

    /// Mark task rename as complete
    async fn finalize_task(&self, task_id: &str) -> Result<()> {
        self.downloader.complete_rename(task_id).await?;
        Ok(())
    }

    /// Send notification with poster image
    ///
    /// Reads the poster file from disk and sends it with the notification.
    /// Falls back to text-only notification if the poster cannot be read.
    /// Uses poster_path as cache key for Telegram file_id caching.
    async fn send_notification_with_poster(&self, title: &str, content: &str, poster_path: &str) {
        // poster_path is like "/posters/xxx.jpg", need to convert to absolute path
        let poster_file = if poster_path.starts_with("/posters/") {
            self.config.posters_path().join(&poster_path[9..]) // Skip "/posters/"
        } else {
            PathBuf::from(poster_path)
        };

        // Try to read the poster file
        match tokio::fs::read(&poster_file).await {
            Ok(photo_data) => {
                // Use poster_path as cache_key for Telegram file_id caching
                self.notification.notify_download_with_photo(
                    title,
                    content,
                    photo_data,
                    Some(poster_path.to_string()),
                );
            }
            Err(e) => {
                tracing::debug!(
                    "Could not read poster file {:?}: {}, falling back to text",
                    poster_file,
                    e
                );
                self.notification.notify_download(title, content);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_episode_range() {
        // Empty
        assert_eq!(RenameService::format_episode_range(&[]), "");

        // Single episode
        assert_eq!(RenameService::format_episode_range(&[1]), "01");
        assert_eq!(RenameService::format_episode_range(&[12]), "12");

        // Consecutive range
        assert_eq!(RenameService::format_episode_range(&[1, 2, 3]), "01-03");
        assert_eq!(
            RenameService::format_episode_range(&[8, 9, 10, 11, 12]),
            "08-12"
        );

        // Non-consecutive
        assert_eq!(
            RenameService::format_episode_range(&[1, 3, 5]),
            "01, 03, 05"
        );

        // Mixed ranges
        assert_eq!(
            RenameService::format_episode_range(&[1, 2, 3, 5, 6]),
            "01-03, 05-06"
        );
        assert_eq!(
            RenameService::format_episode_range(&[1, 2, 5, 6, 7, 10]),
            "01-02, 05-07, 10"
        );
    }
}
