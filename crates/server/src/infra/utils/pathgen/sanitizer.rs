/// Sanitizes path components by removing or replacing filesystem-illegal characters.
///
/// Handles characters that are illegal on Windows and Unix:
/// - Windows: < > : " / \ | ? *
/// - Also handles trailing dots (Windows restriction)
pub struct PathSanitizer;

impl PathSanitizer {
    /// Characters that are illegal in file/directory names on Windows and Unix
    const ILLEGAL_CHARS: &'static [char] = &['<', '>', ':', '"', '/', '\\', '|', '?', '*'];

    /// Sanitize a single path component (file or directory name)
    ///
    /// # Examples
    /// ```
    /// use server::pathgen::PathSanitizer;
    ///
    /// assert_eq!(PathSanitizer::sanitize("Title: Subtitle"), "Title Subtitle");
    /// assert_eq!(PathSanitizer::sanitize("Movie?<>Name"), "Movie Name");
    /// ```
    pub fn sanitize(component: &str) -> String {
        let mut result = component.to_string();

        // Replace illegal characters with space
        for &ch in Self::ILLEGAL_CHARS {
            result = result.replace(ch, " ");
        }

        // Trim leading/trailing whitespace
        result = result.trim().to_string();

        // Compress multiple consecutive spaces into one
        while result.contains("  ") {
            result = result.replace("  ", " ");
        }

        // Remove trailing dots (Windows doesn't allow this)
        result = result.trim_end_matches('.').to_string();

        // Ensure result is not empty
        if result.is_empty() {
            result = "Unknown".to_string();
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sanitize_illegal_chars() {
        assert_eq!(PathSanitizer::sanitize("Title: Subtitle"), "Title Subtitle");
        assert_eq!(PathSanitizer::sanitize("Movie?<>Name"), "Movie Name");
        assert_eq!(
            PathSanitizer::sanitize("Test/Path\\Name"),
            "Test Path Name"
        );
    }

    #[test]
    fn test_sanitize_multiple_spaces() {
        assert_eq!(
            PathSanitizer::sanitize("Title   With   Spaces"),
            "Title With Spaces"
        );
    }

    #[test]
    fn test_sanitize_trailing_dots() {
        assert_eq!(PathSanitizer::sanitize("Filename..."), "Filename");
    }

    #[test]
    fn test_sanitize_empty() {
        assert_eq!(PathSanitizer::sanitize(""), "Unknown");
        assert_eq!(PathSanitizer::sanitize("   "), "Unknown");
        assert_eq!(PathSanitizer::sanitize(":::"), "Unknown");
    }

    #[test]
    fn test_sanitize_chinese_title() {
        assert_eq!(PathSanitizer::sanitize("迷宫饭"), "迷宫饭");
        assert_eq!(PathSanitizer::sanitize("测试: 动画"), "测试 动画");
    }
}
