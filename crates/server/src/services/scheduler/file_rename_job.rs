//! File rename job that processes completed downloads based on tags.
//!
//! This job scans the downloader for completed torrents with the "rename" tag
//! and renames files based on whether they are moe-managed or external tasks.

use async_trait::async_trait;
use downloader::TorrentFile;
use sqlx::SqlitePool;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::repositories::{DownloadTaskRepository, TorrentRepository};
use crate::services::{DownloaderService, FileRenameService};

/// File renaming job that runs periodically.
///
/// This job checks completed downloads with "rename" tag and renames files:
/// - moe-tagged tasks: Use torrent name directly
/// - Non-moe tasks: Parse filename with parser and generate path with pathgen
pub struct FileRenameJob {
    db: SqlitePool,
    downloader: Arc<DownloaderService>,
    /// Sync response ID for incremental updates
    rid: AtomicI64,
}

impl FileRenameJob {
    /// Creates a new file rename job.
    pub fn new(db: SqlitePool, downloader: Arc<DownloaderService>) -> Self {
        Self {
            db,
            downloader,
            rid: AtomicI64::new(0),
        }
    }

    /// Parse tags string (comma-separated) and check if tag exists
    fn has_tag(tags_str: &Option<String>, tag: &str) -> bool {
        tags_str
            .as_ref()
            .is_some_and(|tags| tags.split(',').any(|t| t.trim() == tag))
    }

    /// Check if torrent is completed based on state and progress
    fn is_completed(state: &Option<String>, progress: &Option<f64>) -> bool {
        progress.unwrap_or(0.0) >= 1.0
            || matches!(
                state.as_deref(),
                Some("uploading" | "stalledUP" | "pausedUP" | "forcedUP" | "queuedUP" | "checkingUP")
            )
    }

    /// Find the largest completed video file
    fn find_main_video_file(files: &[TorrentFile]) -> Option<&TorrentFile> {
        files
            .iter()
            .filter(|f| f.is_completed() && f.is_video())
            .max_by_key(|f| f.size)
    }

    /// Process a moe-tagged torrent (uses database records)
    async fn process_moe_torrent(
        &self,
        hash: &str,
        torrent_name: &str,
    ) -> Result<bool, Box<dyn std::error::Error + Send + Sync>> {
        // Look up torrent in database
        let torrent = match TorrentRepository::get_by_info_hash(&self.db, hash).await? {
            Some(t) => t,
            None => {
                tracing::warn!(
                    "Moe torrent {} not found in database, skipping",
                    hash
                );
                return Ok(false);
            }
        };

        // Get the latest download task
        let task =
            match DownloadTaskRepository::get_latest_by_torrent_id(&self.db, torrent.id).await? {
                Some(t) => t,
                None => {
                    tracing::warn!(
                        "No download task found for moe torrent {}, skipping",
                        torrent.id
                    );
                    return Ok(false);
                }
            };

        // Use existing FileRenameService for moe tasks (already has proper pathgen logic)
        match FileRenameService::rename_completed_torrent(
            &self.db,
            &self.downloader,
            &torrent,
            &task,
        )
        .await
        {
            Ok(results) => {
                let success = results.iter().any(|r| {
                    matches!(
                        r,
                        crate::services::RenameResult::Renamed { .. }
                            | crate::services::RenameResult::AlreadyRenamed
                    )
                });
                tracing::info!(
                    "Moe torrent {} ({}) rename completed, success={}",
                    torrent.id,
                    torrent_name,
                    success
                );
                Ok(success)
            }
            Err(e) => {
                tracing::error!(
                    "Failed to rename moe torrent {} ({}): {}",
                    torrent.id,
                    torrent_name,
                    e
                );
                Ok(false)
            }
        }
    }

    /// Process a non-moe torrent (uses parser to analyze filename)
    async fn process_external_torrent(
        &self,
        hash: &str,
        _torrent_name: &str,
    ) -> Result<bool, Box<dyn std::error::Error + Send + Sync>> {
        // Get files in the torrent
        let files = self.downloader.get_task_files(hash).await?;

        // Find the main video file
        let main_file = match Self::find_main_video_file(&files) {
            Some(f) => f,
            None => {
                tracing::warn!(
                    "No completed video file found for external torrent {}, skipping",
                    hash
                );
                return Ok(false);
            }
        };

        // Skip multi-file torrents for now (留空)
        let video_count = files.iter().filter(|f| f.is_video()).count();
        if video_count > 1 {
            tracing::info!(
                "External torrent {} has {} video files, multi-file rename not implemented yet",
                hash,
                video_count
            );
            return Ok(false);
        }

        // Parse the filename
        let filename = main_file.name.rsplit('/').next().unwrap_or(&main_file.name);
        let parser = parser::Parser::new();

        let parse_result = match parser.parse(filename) {
            Ok(r) => r,
            Err(e) => {
                tracing::warn!(
                    "Failed to parse filename '{}' for torrent {}: {}",
                    filename,
                    hash,
                    e
                );
                return Ok(false);
            }
        };

        // Extract episode and season
        let episode = match parse_result.episode {
            Some(e) => e,
            None => {
                tracing::warn!(
                    "No episode number found in filename '{}' for torrent {}",
                    filename,
                    hash
                );
                return Ok(false);
            }
        };

        let season = parse_result.season.unwrap_or(1);

        // Get title (prefer Chinese, then English, then Japanese)
        let title = parse_result
            .name_zh
            .or(parse_result.name_en)
            .or(parse_result.name_jp)
            .unwrap_or_else(|| "Unknown".to_string());

        // Generate new filename using pathgen (simplified without tmdb_id)
        let new_filename_base = pathgen::generate_filename(&title, season, episode, None);
        let extension = main_file.extension().unwrap_or("mkv");
        let new_filename = format!("{}.{}", new_filename_base, extension);

        // Keep the original directory structure
        let new_path = if let Some((dir, _)) = main_file.name.rsplit_once('/') {
            format!("{}/{}", dir, new_filename)
        } else {
            new_filename.clone()
        };

        // Check if already renamed
        if main_file.name.ends_with(&new_filename) {
            tracing::debug!(
                "External torrent {} file already has correct name",
                hash
            );
            return Ok(true);
        }

        // Rename the file
        match self
            .downloader
            .rename_file(hash, &main_file.name, &new_path)
            .await
        {
            Ok(()) => {
                tracing::info!(
                    "Renamed external torrent {} file: {} -> {}",
                    hash,
                    main_file.name,
                    new_path
                );
                Ok(true)
            }
            Err(e) => {
                tracing::error!(
                    "Failed to rename external torrent {} file: {}",
                    hash,
                    e
                );
                Ok(false)
            }
        }
    }
}

#[async_trait]
impl SchedulerJob for FileRenameJob {
    fn name(&self) -> &'static str {
        "FileRename"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(600) // Every 10 minutes
    }

    async fn execute(&self) -> JobResult {
        tracing::debug!("Starting file rename job");

        // Get sync maindata with tags
        let rid = self.rid.load(Ordering::Relaxed);
        let sync_data = match self.downloader.get_tasks_info(rid).await {
            Ok(data) => data,
            Err(e) => {
                tracing::warn!("Failed to get tasks info: {}", e);
                return Ok(());
            }
        };

        // Update rid for next iteration
        self.rid.store(sync_data.rid, Ordering::Relaxed);

        // Filter completed torrents with "rename" tag
        let candidates: Vec<_> = sync_data
            .torrents
            .into_iter()
            .filter(|(_, info)| {
                Self::has_tag(&info.tags, "rename")
                    && Self::is_completed(&info.state, &info.progress)
            })
            .collect();

        if candidates.is_empty() {
            tracing::debug!("No completed torrents with 'rename' tag found");
            return Ok(());
        }

        tracing::info!(
            "Found {} completed torrents with 'rename' tag",
            candidates.len()
        );

        // Process each candidate
        for (hash, info) in candidates {
            let has_moe_tag = Self::has_tag(&info.tags, "moe");
            let torrent_name = info.name.as_deref().unwrap_or("unknown");

            tracing::info!(
                "Processing torrent {} ({}), moe={}",
                hash,
                torrent_name,
                has_moe_tag
            );

            let success = if has_moe_tag {
                self.process_moe_torrent(&hash, torrent_name).await
            } else {
                self.process_external_torrent(&hash, torrent_name).await
            };

            match success {
                Ok(true) => {
                    // Remove "rename" tag on success
                    if let Err(e) = self.downloader.remove_tags(&hash, &["rename"]).await {
                        tracing::warn!("Failed to remove 'rename' tag from {}: {}", hash, e);
                    } else {
                        tracing::debug!("Removed 'rename' tag from {}", hash);
                    }
                }
                Ok(false) => {
                    tracing::debug!("Torrent {} not processed successfully, keeping 'rename' tag", hash);
                }
                Err(e) => {
                    tracing::error!("Error processing torrent {}: {}", hash, e);
                }
            }
        }

        tracing::debug!("File rename job completed");
        Ok(())
    }
}
