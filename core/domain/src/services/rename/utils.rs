//! Shared utility functions for rename operations
//!
//! This module contains path generation, subtitle handling,
//! and other common utilities used by all rename processors.

use std::path::Path;

use parser::Parser;

use crate::models::BangumiWithMetadata;
use crate::services::{DownloaderHandle, Task, TaskFile};

use super::{RenameError, Result};

// ============ Path Generation ============

/// Build destination path for BDRip episode
///
/// Format: `Title (year) {tmdb-ID}/Season XX/filename.ext`
pub(super) fn build_bdrip_path(
    title: &str,
    year: i32,
    tmdb_id: Option<i64>,
    season: i32,
    filename: &str,
    ext: &str,
) -> String {
    let base_dir = format_base_dir(title, year, tmdb_id);
    format!("{}/Season {:02}/{}.{}", base_dir, season, filename, ext)
}

/// Build destination path for special content
///
/// Format: `Title (year) {tmdb-ID}/Specials/filename.ext`
pub(super) fn build_special_path(
    title: &str,
    year: i32,
    tmdb_id: Option<i64>,
    filename: &str,
    ext: &str,
) -> String {
    let base_dir = format_base_dir(title, year, tmdb_id);
    format!("{}/Specials/{}.{}", base_dir, filename, ext)
}

/// Format base directory: "Title (year) {tmdb-ID}"
pub(super) fn format_base_dir(title: &str, year: i32, tmdb_id: Option<i64>) -> String {
    let sanitized = pathgen::sanitize(title);
    if let Some(id) = tmdb_id {
        format!("{} ({}) {{tmdb-{}}}", sanitized, year, id)
    } else {
        format!("{} ({})", sanitized, year)
    }
}

/// Join filename with parent directory, preserving directory structure
pub(super) fn join_with_parent(original_path: &str, new_filename: &str) -> String {
    let path = Path::new(original_path);
    if let Some(parent) = path.parent() {
        if parent.as_os_str().is_empty() {
            new_filename.to_string()
        } else {
            parent.join(new_filename).to_string_lossy().to_string()
        }
    } else {
        new_filename.to_string()
    }
}

// ============ File Processing ============

/// Rename subtitle files that match the video file
pub(super) async fn rename_subtitles(
    downloader: &DownloaderHandle,
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
                .await
                .map_err(RenameError::Downloader)?;
        }
    }

    Ok(())
}

/// Parse episode number from filename using the parser
pub(super) fn parse_episode_number(parser: &Parser, filename: &str) -> Option<i32> {
    // Extract just the filename part
    let name = Path::new(filename)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(filename);

    match parser.parse(name) {
        Ok(result) => result.episode,
        Err(_) => None,
    }
}

// ============ Standard Path Generation ============

/// Generate standard rename path for a video file
///
/// Applies episode offset and generates Plex/Jellyfin compatible path.
pub(super) fn generate_standard_path(
    bangumi: &BangumiWithMetadata,
    old_path: &str,
    episode: i32,
    ext: &str,
) -> String {
    let adjusted_episode = bangumi.metadata.adjust_episode(episode);
    let new_filename_base = pathgen::generate_filename(
        &bangumi.metadata.title_chinese,
        bangumi.metadata.season,
        adjusted_episode,
        Some(bangumi.metadata.platform.as_str()),
    );
    let new_filename = format!("{}.{}", new_filename_base, ext);
    join_with_parent(old_path, &new_filename)
}

/// Generate filename base (without extension) for standard rename
pub(super) fn generate_filename_base(bangumi: &BangumiWithMetadata, episode: i32) -> String {
    let adjusted_episode = bangumi.metadata.adjust_episode(episode);
    pathgen::generate_filename(
        &bangumi.metadata.title_chinese,
        bangumi.metadata.season,
        adjusted_episode,
        Some(bangumi.metadata.platform.as_str()),
    )
}

// ============ BDRip Rename Operations ============

/// Rename a BDRip episode file
///
/// Note: BDRip episode numbers are NOT adjusted with episode_offset because:
/// 1. BDRip releases typically use season-relative numbering (e.g., S2E01, not S2E13)
/// 2. The episode number is parsed from directory structure, not RSS metadata
pub(super) async fn rename_bdrip_episode(
    downloader: &DownloaderHandle,
    task: &Task,
    file: &TaskFile,
    bangumi: &BangumiWithMetadata,
    season: i32,
    episode: i32,
    all_files: &[TaskFile],
) -> Result<()> {
    let ext = file.extension().unwrap_or("mkv");

    // BDRip episode numbers are already season-relative, no offset needed
    let new_filename_base = pathgen::generate_filename(
        &bangumi.metadata.title_chinese,
        season,
        episode,
        Some(bangumi.metadata.platform.as_str()),
    );

    let new_path = build_bdrip_path(
        &bangumi.metadata.title_chinese,
        bangumi.metadata.year,
        bangumi.metadata.tmdb_id,
        season,
        &new_filename_base,
        ext,
    );

    // Rename subtitles first
    rename_subtitles(downloader, task, &file.path, &new_filename_base, all_files).await?;

    // Rename video file
    if file.path != new_path {
        tracing::info!("BDRip rename: {} -> {}", file.path, new_path);
        downloader
            .rename_file(&task.id, &file.path, &new_path)
            .await?;
    }

    Ok(())
}

/// Rename a special/SP file
pub(super) async fn rename_bdrip_special(
    downloader: &DownloaderHandle,
    task: &Task,
    file: &TaskFile,
    bangumi: &BangumiWithMetadata,
    sp_number: i32,
    all_files: &[TaskFile],
) -> Result<()> {
    let ext = file.extension().unwrap_or("mkv");

    // Generate special filename: Title - s00eXX
    let title = pathgen::sanitize(&bangumi.metadata.title_chinese);
    let new_filename_base = format!("{} - s00e{:02}", title, sp_number);

    let new_path = build_special_path(
        &bangumi.metadata.title_chinese,
        bangumi.metadata.year,
        bangumi.metadata.tmdb_id,
        &new_filename_base,
        ext,
    );

    // Rename subtitles first
    rename_subtitles(downloader, task, &file.path, &new_filename_base, all_files).await?;

    // Rename video file
    if file.path != new_path {
        tracing::info!("Special rename: {} -> {}", file.path, new_path);
        downloader
            .rename_file(&task.id, &file.path, &new_path)
            .await?;
    }

    Ok(())
}
