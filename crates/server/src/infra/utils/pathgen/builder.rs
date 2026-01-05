use super::{DefaultFormatter, PathFormatter, PathInfo, Result};
use std::path::PathBuf;

/// Builder for constructing episode download paths
///
/// Combines a base path with a formatter to generate complete paths.
pub struct PathBuilder {
    base_path: Option<PathBuf>,
    formatter: Box<dyn PathFormatter>,
}

impl PathBuilder {
    /// Create a new path builder with the default formatter
    pub fn new() -> Self {
        Self {
            base_path: None,
            formatter: Box::new(DefaultFormatter::new()),
        }
    }

    /// Set the base path for generated paths
    pub fn with_base_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.base_path = Some(path.into());
        self
    }

    /// Set a custom formatter
    pub fn with_formatter(mut self, formatter: impl PathFormatter + 'static) -> Self {
        self.formatter = Box::new(formatter);
        self
    }

    /// Build the complete path including base path
    pub fn build(&self, info: &PathInfo) -> Result<PathBuf> {
        let relative_path = self.formatter.format(info)?;

        Ok(if let Some(base) = &self.base_path {
            base.join(relative_path)
        } else {
            relative_path
        })
    }

    /// Build only the directory path (without filename)
    pub fn build_directory(&self, info: &PathInfo) -> Result<PathBuf> {
        let relative_path = self.formatter.format_directory(info)?;

        Ok(if let Some(base) = &self.base_path {
            base.join(relative_path)
        } else {
            relative_path
        })
    }

    /// Build only the filename (without extension)
    pub fn build_filename(&self, info: &PathInfo) -> Result<String> {
        self.formatter.format_filename(info)
    }
}

impl Default for PathBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_with_base_path() {
        let info = PathInfo::new("测试动画", 2024, 1, 5).with_tmdb_id(12345);

        let path = PathBuilder::new()
            .with_base_path("/Media/Bangumi")
            .build(&info)
            .unwrap();

        assert_eq!(
            path.to_str().unwrap(),
            "/Media/Bangumi/测试动画 (2024) {tmdb-12345}/Season 01/测试动画 - s01e05"
        );
    }

    #[test]
    fn test_build_without_base_path() {
        let info = PathInfo::new("测试动画", 2024, 1, 5).with_tmdb_id(12345);

        let path = PathBuilder::new().build(&info).unwrap();

        assert_eq!(
            path.to_str().unwrap(),
            "测试动画 (2024) {tmdb-12345}/Season 01/测试动画 - s01e05"
        );
    }

    #[test]
    fn test_build_directory_only() {
        let info = PathInfo::new("测试动画", 2024, 2, 10).with_tmdb_id(12345);

        let dir = PathBuilder::new()
            .with_base_path("/Media/Bangumi")
            .build_directory(&info)
            .unwrap();

        assert_eq!(
            dir.to_str().unwrap(),
            "/Media/Bangumi/测试动画 (2024) {tmdb-12345}/Season 02"
        );
    }

    #[test]
    fn test_build_movie() {
        let info = PathInfo::new("测试电影", 2024, 1, 1)
            .with_kind("movie")
            .with_tmdb_id(99999);

        let path = PathBuilder::new()
            .with_base_path("/Media/Movies")
            .build(&info)
            .unwrap();

        assert_eq!(
            path.to_str().unwrap(),
            "/Media/Movies/测试电影 (2024) {tmdb-99999}/测试电影"
        );
    }

    #[test]
    fn test_build_filename_only() {
        let info = PathInfo::new("测试动画", 2024, 1, 12);

        let filename = PathBuilder::new().build_filename(&info).unwrap();

        assert_eq!(filename, "测试动画 - s01e12");
    }
}
