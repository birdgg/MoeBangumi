//! Subtitle file handling for rename operations.
//!
//! Handles renaming of subtitle files that accompany video files.

use std::path::Path;
use std::sync::Arc;

use crate::services::{DownloaderHandle, Task, TaskFile};

use super::paths::join_with_parent;
use super::Result;

/// Rename subtitle files that match the video file
pub async fn rename_subtitles(
    downloader: &Arc<DownloaderHandle>,
    task: &Task,
    old_video_path: &str,
    new_basename: &str,
    all_files: &[TaskFile],
) -> Result<()> {
    // Get the basename of the old video (without extension)
    let old_basename = Path::new(old_video_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("");

    if old_basename.is_empty() {
        return Ok(());
    }

    // Find subtitle files with matching basename
    let subtitle_exts = ["ass", "srt", "ssa", "sub", "vtt"];

    for file in all_files {
        let file_path = Path::new(&file.path);

        // Check if it's a subtitle file
        let is_subtitle = file_path
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| subtitle_exts.contains(&e.to_lowercase().as_str()))
            .unwrap_or(false);

        if !is_subtitle {
            continue;
        }

        // Check if it matches our video file
        // Subtitle files might be named like:
        // - video.ass
        // - video.zh-CN.ass
        // - video.简体中文.ass
        let file_name = file_path.file_name().and_then(|s| s.to_str()).unwrap_or("");

        if !file_name.starts_with(old_basename) {
            continue;
        }

        // Extract the suffix (language tag + extension)
        let suffix = &file_name[old_basename.len()..];
        let new_subtitle_name = format!("{}{}", new_basename, suffix);

        // Build the new path preserving directory
        let new_subtitle_path = join_with_parent(&file.path, &new_subtitle_name);

        if file.path != new_subtitle_path {
            tracing::info!("Renaming subtitle: {} -> {}", file.path, new_subtitle_path);
            downloader
                .rename_file(&task.id, &file.path, &new_subtitle_path)
                .await?;
        }
    }

    Ok(())
}
