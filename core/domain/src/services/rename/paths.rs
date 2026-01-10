//! Path generation utilities for media file renaming.
//!
//! Provides functions for building Plex/Jellyfin compatible directory
//! structures and file paths.

use std::path::Path;

/// Build destination path for BDRip episode
///
/// Format: "Title (year) {tmdb-ID}/Season XX/filename.ext"
pub fn build_bdrip_path(
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
/// Format: "Title (year) {tmdb-ID}/Specials/filename.ext"
pub fn build_special_path(
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
pub fn format_base_dir(title: &str, year: i32, tmdb_id: Option<i64>) -> String {
    let sanitized = pathgen::sanitize(title);
    if let Some(id) = tmdb_id {
        format!("{} ({}) {{tmdb-{}}}", sanitized, year, id)
    } else {
        format!("{} ({})", sanitized, year)
    }
}

/// Join filename with parent directory, preserving directory structure
pub fn join_with_parent(original_path: &str, new_filename: &str) -> String {
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

/// Format episode numbers into a readable range string
///
/// Examples:
/// - [1] -> "01"
/// - [1, 2, 3] -> "01-03"
/// - [1, 3, 5] -> "01, 03, 05"
/// - [1, 2, 3, 5, 6] -> "01-03, 05-06"
pub fn format_episode_range(episodes: &[i32]) -> String {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_bdrip_path_with_tmdb() {
        let path = build_bdrip_path(
            "Re:Zero",
            2016,
            Some(63926),
            1,
            "Re Zero - s01e01",
            "mkv",
        );
        // Note: title is sanitized (colon removed)
        assert_eq!(
            path,
            "Re Zero (2016) {tmdb-63926}/Season 01/Re Zero - s01e01.mkv"
        );
    }

    #[test]
    fn test_build_bdrip_path_without_tmdb() {
        let path = build_bdrip_path("Some Anime", 2023, None, 2, "Some Anime - s02e05", "mkv");
        assert_eq!(path, "Some Anime (2023)/Season 02/Some Anime - s02e05.mkv");
    }

    #[test]
    fn test_build_bdrip_path_different_seasons() {
        // Season 1 path
        let path_s1 = build_bdrip_path("Re:Zero", 2016, Some(63926), 1, "Re:Zero - s01e01", "mkv");
        assert!(path_s1.contains("Season 01"));
        assert!(path_s1.contains("2016"));

        // Season 2 path (different year)
        let path_s2 = build_bdrip_path("Re:Zero", 2020, Some(63926), 2, "Re:Zero - s02e01", "mkv");
        assert!(path_s2.contains("Season 02"));
        assert!(path_s2.contains("2020"));
    }

    #[test]
    fn test_build_special_path() {
        let path = build_special_path("Re:Zero", 2016, Some(63926), "Re Zero - s00e01", "mkv");
        // Note: title is sanitized (colon removed)
        assert_eq!(
            path,
            "Re Zero (2016) {tmdb-63926}/Specials/Re Zero - s00e01.mkv"
        );
    }

    #[test]
    fn test_format_base_dir_with_tmdb() {
        let dir = format_base_dir("Re:Zero", 2016, Some(63926));
        // Note: title is sanitized (colon removed)
        assert_eq!(dir, "Re Zero (2016) {tmdb-63926}");
    }

    #[test]
    fn test_format_base_dir_without_tmdb() {
        let dir = format_base_dir("Some Anime", 2023, None);
        assert_eq!(dir, "Some Anime (2023)");
    }

    #[test]
    fn test_format_base_dir_sanitizes_title() {
        // Test that special characters are sanitized
        let dir = format_base_dir("Test/Anime:Title", 2023, None);
        // Should not contain / or : in the title part
        assert!(!dir.contains("Test/Anime"));
    }

    #[test]
    fn test_join_with_parent() {
        // With parent directory
        assert_eq!(
            join_with_parent("Season 01/old.mkv", "new.mkv"),
            "Season 01/new.mkv"
        );

        // Nested directory
        assert_eq!(
            join_with_parent("Title/Season 01/old.mkv", "new.mkv"),
            "Title/Season 01/new.mkv"
        );

        // No parent directory
        assert_eq!(join_with_parent("old.mkv", "new.mkv"), "new.mkv");
    }

    #[test]
    fn test_format_episode_range() {
        // Empty
        assert_eq!(format_episode_range(&[]), "");

        // Single episode
        assert_eq!(format_episode_range(&[1]), "01");
        assert_eq!(format_episode_range(&[12]), "12");

        // Consecutive range
        assert_eq!(format_episode_range(&[1, 2, 3]), "01-03");
        assert_eq!(format_episode_range(&[8, 9, 10, 11, 12]), "08-12");

        // Non-consecutive
        assert_eq!(format_episode_range(&[1, 3, 5]), "01, 03, 05");

        // Mixed ranges
        assert_eq!(format_episode_range(&[1, 2, 3, 5, 6]), "01-03, 05-06");
        assert_eq!(format_episode_range(&[1, 2, 5, 6, 7, 10]), "01-02, 05-07, 10");
    }
}
