use super::{PathInfo, Result, sanitizer::PathSanitizer};
use std::path::PathBuf;

/// Trait for formatting episode download paths
///
/// Implementations can provide different path formatting strategies.
pub trait PathFormatter: Send + Sync {
    /// Generate the complete relative path (without base_path)
    fn format(&self, info: &PathInfo) -> Result<PathBuf>;

    /// Generate just the directory path (without filename)
    fn format_directory(&self, info: &PathInfo) -> Result<PathBuf>;

    /// Generate just the filename (without extension)
    fn format_filename(&self, info: &PathInfo) -> Result<String>;
}

/// Default path formatter implementing Plex/Jellyfin compatible naming
///
/// Format for TV shows:
/// `{title} ({year}) {{tmdb-ID}}/Season XX/{title} - sXXeYY`
///
/// Format for movies:
/// `{title} ({year}) {{tmdb-ID}}/{title}`
#[derive(Debug, Clone, Default)]
pub struct DefaultFormatter;

impl DefaultFormatter {
    /// Create a new default formatter
    pub fn new() -> Self {
        Self
    }

    /// Format the base directory name: "迷宫饭 (2024) {tmdb-119121}"
    fn format_base_dir(&self, info: &PathInfo) -> String {
        let title = PathSanitizer::sanitize(&info.title);

        if let Some(tmdb_id) = info.tmdb_id {
            format!("{} ({}) {{tmdb-{}}}", title, info.year, tmdb_id)
        } else {
            format!("{} ({})", title, info.year)
        }
    }

    /// Format the season directory: "Season 01"
    fn format_season_dir(&self, season: i32) -> String {
        format!("Season {:02}", season)
    }

    /// Format the episode filename: "迷宫饭 - s01e12"
    fn format_episode_filename(&self, info: &PathInfo) -> String {
        let title = PathSanitizer::sanitize(&info.title);
        format!("{} - s{:02}e{:02}", title, info.season, info.episode)
    }

    /// Format the movie filename: "迷宫饭"
    fn format_movie_filename(&self, info: &PathInfo) -> String {
        PathSanitizer::sanitize(&info.title)
    }
}

impl PathFormatter for DefaultFormatter {
    fn format(&self, info: &PathInfo) -> Result<PathBuf> {
        let mut path = PathBuf::new();

        // Add base directory
        path.push(self.format_base_dir(info));

        // Movies don't need Season directory
        if !info.is_movie() {
            path.push(self.format_season_dir(info.season));
        }

        // Add filename
        path.push(self.format_filename(info)?);

        Ok(path)
    }

    fn format_directory(&self, info: &PathInfo) -> Result<PathBuf> {
        let mut path = PathBuf::new();
        path.push(self.format_base_dir(info));

        if !info.is_movie() {
            path.push(self.format_season_dir(info.season));
        }

        Ok(path)
    }

    fn format_filename(&self, info: &PathInfo) -> Result<String> {
        Ok(if info.is_movie() {
            self.format_movie_filename(info)
        } else {
            self.format_episode_filename(info)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_tv_show() {
        let formatter = DefaultFormatter::new();
        let info = PathInfo::new("迷宫饭", 2024, 1, 12).with_tmdb_id(119121);

        let path = formatter.format(&info).unwrap();
        assert_eq!(
            path.to_str().unwrap(),
            "迷宫饭 (2024) {tmdb-119121}/Season 01/迷宫饭 - s01e12"
        );
    }

    #[test]
    fn test_format_movie() {
        let formatter = DefaultFormatter::new();
        let info = PathInfo::new("电影名称", 2024, 1, 1)
            .with_kind("movie")
            .with_tmdb_id(12345);

        let path = formatter.format(&info).unwrap();
        assert_eq!(
            path.to_str().unwrap(),
            "电影名称 (2024) {tmdb-12345}/电影名称"
        );
    }

    #[test]
    fn test_format_directory_only() {
        let formatter = DefaultFormatter::new();
        let info = PathInfo::new("测试动画", 2024, 2, 5).with_tmdb_id(99999);

        let dir = formatter.format_directory(&info).unwrap();
        assert_eq!(
            dir.to_str().unwrap(),
            "测试动画 (2024) {tmdb-99999}/Season 02"
        );
    }

    #[test]
    fn test_format_without_tmdb_id() {
        let formatter = DefaultFormatter::new();
        let info = PathInfo::new("没有ID的动画", 2024, 1, 1);

        let path = formatter.format(&info).unwrap();
        assert_eq!(
            path.to_str().unwrap(),
            "没有ID的动画 (2024)/Season 01/没有ID的动画 - s01e01"
        );
    }

    #[test]
    fn test_format_with_special_chars() {
        let formatter = DefaultFormatter::new();
        let info = PathInfo::new("Title: Subtitle?", 2024, 1, 1).with_tmdb_id(123);

        let path = formatter.format(&info).unwrap();
        assert_eq!(
            path.to_str().unwrap(),
            "Title Subtitle (2024) {tmdb-123}/Season 01/Title Subtitle - s01e01"
        );
    }
}
