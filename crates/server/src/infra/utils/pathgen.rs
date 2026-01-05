//! Episode download path generator for media organization
//!
//! This module provides utilities for generating well-organized paths for downloaded
//! media files, compatible with media servers like Plex and Jellyfin.
//!
//! # Features
//!
//! - Sanitizes path components to remove filesystem-illegal characters
//! - Supports different content types (tv, movie, ova)
//! - Plex/Jellyfin compatible naming scheme
//! - Extensible formatter trait for custom path formats

mod builder;
mod error;
mod formatter;
mod models;
mod sanitizer;

pub use builder::PathBuilder;
pub use error::{PathGenError, Result};
pub use formatter::{DefaultFormatter, PathFormatter};
pub use models::PathInfo;
pub use sanitizer::PathSanitizer;

/// Convenience function for generating a complete path
///
/// This is a shorthand for using `PathBuilder` with common parameters.
///
/// # Arguments
///
/// * `base_path` - The base directory path (e.g., "/Media/Bangumi")
/// * `title` - The title of the media (will be sanitized)
/// * `year` - The release year
/// * `season` - The season number
/// * `episode` - The episode number
/// * `tmdb_id` - Optional TMDB ID for the media
/// * `kind` - Optional content type ("tv", "movie", "ova")
///
pub fn generate_path(
    base_path: &str,
    title: &str,
    year: i32,
    season: i32,
    episode: i32,
    tmdb_id: Option<i64>,
    kind: Option<&str>,
) -> Result<String> {
    let mut info = PathInfo::new(title, year, season, episode);

    if let Some(id) = tmdb_id {
        info = info.with_tmdb_id(id);
    }

    if let Some(k) = kind {
        info = info.with_kind(k);
    }

    let path = PathBuilder::new().with_base_path(base_path).build(&info)?;

    Ok(path.to_string_lossy().to_string())
}

/// Convenience function for generating only the directory path (without filename)
///
/// Useful when you only need the save directory for a torrent download.
pub fn generate_directory(
    base_path: &str,
    title: &str,
    year: i32,
    season: i32,
    tmdb_id: Option<i64>,
    kind: Option<&str>,
) -> Result<String> {
    let mut info = PathInfo::new(title, year, season, 0);

    if let Some(id) = tmdb_id {
        info = info.with_tmdb_id(id);
    }

    if let Some(k) = kind {
        info = info.with_kind(k);
    }

    let path = PathBuilder::new()
        .with_base_path(base_path)
        .build_directory(&info)?;

    Ok(path.to_string_lossy().to_string())
}

/// Convenience function for generating only the filename (without extension)
///
/// Useful when you need to rename a torrent's content.
pub fn generate_filename(title: &str, season: i32, episode: i32, kind: Option<&str>) -> String {
    let mut info = PathInfo::new(title, 2000, season, episode); // year is not used for filename

    if let Some(k) = kind {
        info = info.with_kind(k);
    }

    PathBuilder::new()
        .build_filename(&info)
        .unwrap_or_else(|_| format!("{} - s{:02}e{:02}", title, season, episode))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_path_tv() {
        let path =
            generate_path("/Media/Bangumi", "测试动画", 2024, 1, 5, Some(12345), Some("tv"))
                .unwrap();
        assert_eq!(
            path,
            "/Media/Bangumi/测试动画 (2024) {tmdb-12345}/Season 01/测试动画 - s01e05"
        );
    }

    #[test]
    fn test_generate_path_movie() {
        let path = generate_path(
            "/Media/Movies",
            "测试电影",
            2024,
            1,
            1,
            Some(99999),
            Some("movie"),
        )
        .unwrap();
        assert_eq!(path, "/Media/Movies/测试电影 (2024) {tmdb-99999}/测试电影");
    }

    #[test]
    fn test_generate_directory() {
        let dir =
            generate_directory("/Media/Bangumi", "测试动画", 2024, 2, Some(12345), Some("tv"))
                .unwrap();
        assert_eq!(dir, "/Media/Bangumi/测试动画 (2024) {tmdb-12345}/Season 02");
    }

    #[test]
    fn test_generate_path_without_tmdb() {
        let path = generate_path("/Media/Bangumi", "没有ID", 2024, 1, 1, None, None).unwrap();
        assert_eq!(
            path,
            "/Media/Bangumi/没有ID (2024)/Season 01/没有ID - s01e01"
        );
    }
}
