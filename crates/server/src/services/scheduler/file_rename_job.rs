use async_trait::async_trait;
use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::models::DownloadTask;
use crate::repositories::{BangumiRepository, DownloadTaskRepository, TorrentRepository};
use crate::services::DownloaderService;

/// File renaming job that runs periodically.
///
/// This job checks for completed downloads and renames the files
/// according to the Plex/Jellyfin compatible naming scheme.
pub struct FileRenameJob {
    db: SqlitePool,
    downloader: Arc<DownloaderService>,
}

impl FileRenameJob {
    /// Creates a new file rename job.
    pub fn new(db: SqlitePool, downloader: Arc<DownloaderService>) -> Self {
        Self { db, downloader }
    }
}

#[async_trait]
impl SchedulerJob for FileRenameJob {
    fn name(&self) -> &'static str {
        "FileRename"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(600) // Every 10 minutes (fallback for webhook)
    }

    async fn execute(&self) -> JobResult {
        tracing::debug!("Starting file rename job");

        // Get all downloading tasks
        let tasks = DownloadTaskRepository::get_downloading(&self.db).await?;

        if tasks.is_empty() {
            tracing::debug!("No downloading tasks found");
            return Ok(());
        }

        tracing::info!("Checking {} downloading tasks for completion", tasks.len());

        for task in tasks {
            if let Err(e) = self.process_task(&task).await {
                tracing::error!("Failed to process task {}: {}", task.id, e);
                // Continue processing other tasks
            }
        }

        tracing::debug!("File rename job completed");
        Ok(())
    }
}

impl FileRenameJob {
    /// Process a single download task
    async fn process_task(
        &self,
        task: &DownloadTask,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // Get the torrent record
        let torrent = TorrentRepository::get_by_id(&self.db, task.torrent_id)
            .await?
            .ok_or_else(|| format!("Torrent {} not found", task.torrent_id))?;

        // Check torrent status in qBittorrent
        let info = match self.downloader.get_task_info(&torrent.info_hash).await {
            Ok(Some(info)) => info,
            Ok(None) => {
                tracing::warn!(
                    "Torrent {} not found in downloader, skipping",
                    torrent.info_hash
                );
                return Ok(());
            }
            Err(e) => {
                tracing::warn!(
                    "Failed to get torrent info for {}: {}",
                    torrent.info_hash,
                    e
                );
                return Ok(());
            }
        };

        // Check if download is completed
        if !info.is_completed() {
            tracing::debug!(
                "Task {} not completed yet (progress: {:.1}%, state: {})",
                task.id,
                info.progress * 100.0,
                info.state
            );
            return Ok(());
        }

        tracing::info!("Task {} completed, starting file rename process", task.id);

        // Get bangumi info for path generation
        let bangumi = BangumiRepository::get_by_id(&self.db, torrent.bangumi_id)
            .await?
            .ok_or_else(|| format!("Bangumi {} not found", torrent.bangumi_id))?;

        // Get files in the torrent
        let files = self
            .downloader
            .get_task_files(&torrent.info_hash)
            .await?;

        // Find the main video file (largest completed video file)
        let main_file = files
            .iter()
            .filter(|f| f.is_completed() && f.is_video())
            .max_by_key(|f| f.size);

        let Some(main_file) = main_file else {
            tracing::warn!(
                "No completed video file found for task {}, marking as completed anyway",
                task.id
            );
            DownloadTaskRepository::mark_completed(&self.db, task.id).await?;
            return Ok(());
        };

        // Generate target filename using pathgen
        let target_filename = pathgen::generate_filename(
            &bangumi.title_chinese,
            bangumi.season,
            torrent.episode_number,
            bangumi.kind.as_deref(),
        );

        // Preserve the original extension
        let extension = main_file.extension().unwrap_or("mkv");
        let new_filename = format!("{}.{}", target_filename, extension);

        // Build the new path preserving directory structure
        // qBittorrent's renameFile API requires the new path to be relative to torrent root
        let new_path = if let Some((dir, _)) = main_file.name.rsplit_once('/') {
            format!("{}/{}", dir, new_filename)
        } else {
            new_filename.clone()
        };

        // Check if already renamed (filename matches target)
        if main_file.name.ends_with(&new_filename) {
            tracing::debug!(
                "File already has correct name for task {}, marking as completed",
                task.id
            );
            DownloadTaskRepository::mark_completed(&self.db, task.id).await?;
            return Ok(());
        }

        // Rename the file
        match self
            .downloader
            .rename_file(&torrent.info_hash, &main_file.name, &new_path)
            .await
        {
            Ok(()) => {
                tracing::info!(
                    "Renamed file for task {}: {} -> {}",
                    task.id,
                    main_file.name,
                    new_path
                );
                DownloadTaskRepository::mark_completed(&self.db, task.id).await?;
            }
            Err(e) => {
                tracing::error!("Failed to rename file for task {}: {}", task.id, e);
                DownloadTaskRepository::mark_failed(&self.db, task.id, &e.to_string()).await?;
            }
        }

        Ok(())
    }
}
