//! Title cleaning utilities for search optimization

use regex::Regex;
use std::sync::LazyLock;

// Pattern to match split-cour markers: 第Xクール, 第X部分, Part X
static SPLIT_COUR_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)(?:第\s*\d+\s*(?:クール|部分))|(?:Part\s*\d+)").unwrap()
});

// Pattern to match season markers: 第X季, 第X期, SX, Season X (case insensitive)
// Split into explicit patterns for clarity: SEASON X, S01
static SEASON_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?i)(?:第[0-9一二三四五六七八九十]+(?:季|期))|(?:SEASON\s*\d{1,2})|(?:S\d{1,2})",
    )
    .unwrap()
});

/// Clean title for TMDB search by removing season and split-cour markers.
///
/// Examples:
/// - "SPY×FAMILY 第2クール" -> "SPY×FAMILY"
/// - "魔法使的新娘 第二季 第2部分" -> "魔法使的新娘"
/// - "葬送的芙莉莲 第1期" -> "葬送的芙莉莲"
/// - "Frieren S02" -> "Frieren"
/// - "Attack on Titan Part 2" -> "Attack on Titan"
pub fn clean_title_for_search(title: &str) -> String {
    let mut result = title.to_string();

    // Remove split-cour markers (第Xクール, 第X部分, Part X)
    result = SPLIT_COUR_PATTERN.replace_all(&result, "").to_string();

    // Remove season markers (第X季, 第X期, SX, Season X)
    result = SEASON_PATTERN.replace_all(&result, "").to_string();

    // Clean up extra whitespace
    result.split_whitespace().collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clean_title_for_search() {
        // Split-cour markers (クール)
        assert_eq!(clean_title_for_search("SPY×FAMILY 第2クール"), "SPY×FAMILY");
        assert_eq!(
            clean_title_for_search("魔法使いの嫁 SEASON2 第2クール"),
            "魔法使いの嫁"
        );

        // Split-cour markers (部分)
        assert_eq!(clean_title_for_search("间谍过家家 第2部分"), "间谍过家家");
        assert_eq!(
            clean_title_for_search("魔法使的新娘 第二季 第2部分"),
            "魔法使的新娘"
        );

        // English Part marker
        assert_eq!(
            clean_title_for_search("Attack on Titan Part 2"),
            "Attack on Titan"
        );

        // Season markers (中文)
        assert_eq!(clean_title_for_search("葬送的芙莉莲 第1期"), "葬送的芙莉莲");
        assert_eq!(clean_title_for_search("我推的孩子 第二季"), "我推的孩子");

        // Season markers (英文)
        assert_eq!(clean_title_for_search("Frieren S02"), "Frieren");
        assert_eq!(
            clean_title_for_search("Spy x Family Season 2"),
            "Spy x Family"
        );
        assert_eq!(
            clean_title_for_search("Attack on Titan SEASON3"),
            "Attack on Titan"
        );

        // No markers - should remain unchanged
        assert_eq!(
            clean_title_for_search("葬送のフリーレン"),
            "葬送のフリーレン"
        );
        assert_eq!(clean_title_for_search("SPY×FAMILY"), "SPY×FAMILY");

        // Edge case: multiple spaces after removal
        assert_eq!(
            clean_title_for_search("魔法使いの嫁  第2クール  "),
            "魔法使いの嫁"
        );

        // Edge case: multiple season/cour markers
        assert_eq!(
            clean_title_for_search("魔法使的新娘 第2季 第2クール"),
            "魔法使的新娘"
        );

        // Edge case: season marker at the start (rare but possible)
        assert_eq!(clean_title_for_search("S02 Frieren"), "Frieren");
    }
}
