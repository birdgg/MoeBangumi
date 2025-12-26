use axum::extract::{Query, State};
use serde::Deserialize;
use utoipa::IntoParams;

use crate::error::{AppError, AppResult};
use crate::repositories::{BangumiRepository, DownloadTaskRepository, TorrentRepository};
use crate::state::AppState;

/// Query parameters for torrent completion webhook
#[derive(Debug, Deserialize, IntoParams)]
pub struct TorrentCompletedQuery {
    /// Torrent info hash from qBittorrent
    pub hash: String,
}

/// Webhook endpoint for qBittorrent torrent completion callback.
///
/// Configure qBittorrent to call this endpoint when a torrent finishes downloading:
/// Settings -> Downloads -> "Run external program on torrent finished"
/// Command: `curl -X POST "http://your-server:3000/api/webhook/torrent-completed?hash=%I"`
///
/// The `%I` placeholder will be replaced with the torrent's info hash.
#[utoipa::path(
    post,
    path = "/api/webhook/torrent-completed",
    tag = "webhook",
    params(TorrentCompletedQuery),
    responses(
        (status = 200, description = "File renamed successfully"),
        (status = 404, description = "Torrent not found"),
        (status = 500, description = "Failed to process torrent")
    )
)]
pub async fn torrent_completed(
    State(state): State<AppState>,
    Query(query): Query<TorrentCompletedQuery>,
) -> AppResult<&'static str> {
    let hash = query.hash.to_lowercase();
    tracing::info!("Received torrent completion webhook for hash: {}", hash);

    // Find the torrent by info_hash
    let torrent = TorrentRepository::get_by_info_hash(&state.db, &hash)
        .await
        .map_err(|e| AppError::internal(e.to_string()))?
        .ok_or_else(|| {
            tracing::warn!("Torrent not found for hash: {}", hash);
            AppError::not_found(format!("Torrent with hash {} not found", hash))
        })?;

    // Find the downloading task for this torrent
    let task = DownloadTaskRepository::get_latest_by_torrent_id(&state.db, torrent.id)
        .await
        .map_err(|e| AppError::internal(e.to_string()))?
        .ok_or_else(|| {
            tracing::warn!("No download task found for torrent: {}", torrent.id);
            AppError::not_found(format!("No download task for torrent {}", torrent.id))
        })?;

    // Skip if already completed
    if task.status == crate::models::DownloadTaskStatus::Completed {
        tracing::debug!("Task {} already completed, skipping", task.id);
        return Ok("Already completed");
    }

    // Get bangumi info for path generation
    let bangumi = BangumiRepository::get_by_id(&state.db, torrent.bangumi_id)
        .await
        .map_err(|e| AppError::internal(e.to_string()))?
        .ok_or_else(|| {
            AppError::internal(format!("Bangumi {} not found", torrent.bangumi_id))
        })?;

    // Get files in the torrent
    let files = state
        .downloader
        .get_torrent_files(&hash)
        .await
        .map_err(|e| AppError::internal(e.to_string()))?;

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
        DownloadTaskRepository::mark_completed(&state.db, task.id)
            .await
            .map_err(|e| AppError::internal(e.to_string()))?;
        return Ok("Completed (no video file)");
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
    let new_path = if let Some((dir, _)) = main_file.name.rsplit_once('/') {
        format!("{}/{}", dir, new_filename)
    } else {
        new_filename.clone()
    };

    // Check if already renamed
    if main_file.name.ends_with(&new_filename) {
        tracing::debug!(
            "File already has correct name for task {}, marking as completed",
            task.id
        );
        DownloadTaskRepository::mark_completed(&state.db, task.id)
            .await
            .map_err(|e| AppError::internal(e.to_string()))?;
        return Ok("Already renamed");
    }

    // Rename the file
    match state
        .downloader
        .rename_file(&hash, &main_file.name, &new_path)
        .await
    {
        Ok(()) => {
            tracing::info!(
                "Renamed file for task {}: {} -> {}",
                task.id,
                main_file.name,
                new_path
            );
            DownloadTaskRepository::mark_completed(&state.db, task.id)
                .await
                .map_err(|e| AppError::internal(e.to_string()))?;
            Ok("File renamed successfully")
        }
        Err(e) => {
            tracing::error!("Failed to rename file for task {}: {}", task.id, e);
            DownloadTaskRepository::mark_failed(&state.db, task.id, &e.to_string())
                .await
                .map_err(|e| AppError::internal(e.to_string()))?;
            Err(AppError::internal(format!("Failed to rename file: {}", e)))
        }
    }
}
