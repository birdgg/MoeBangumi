//! BDRip content parser
//!
//! Parses BDRip directory structures to extract season, episode numbers,
//! and content types (Episode, Special, NonVideo).

use regex::Regex;
use std::sync::LazyLock;

use crate::models::CHINESE_NUMBER_MAP;

/// BDRip content type classification
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BDRipContentType {
    /// Main episode (本篇)
    Episode,
    /// Special content (SP, OVA, OAD, etc.)
    Special,
    /// Non-video content (CDs, Scans, etc.) - should be skipped
    NonVideo,
}

/// Result of parsing a BDRip file path
#[derive(Debug, Clone)]
pub struct BDRipParseResult {
    /// Content type classification
    pub content_type: BDRipContentType,
    /// Season number extracted from directory name
    pub season: Option<i32>,
    /// Episode or special number
    pub number: Option<i32>,
    /// Original file path
    pub original_path: String,
}

/// Season pattern: "2nd Season", "Season 2", "第二季", "第十一季", etc.
static SEASON_DIR_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    // Note: Chinese number matching includes compound numbers like 十一, 十二, etc.
    Regex::new(r"(?i)(?:(\d{1,2})(?:st|nd|rd|th)\s*Season|Season\s*(\d+)|第([一二三四五六七八九十]+)[季期])")
        .expect("Invalid season pattern")
});

/// Non-video directory pattern: CDs, Scans, Booklets, etc.
static NON_VIDEO_DIR_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)^(CDs?|Scans?|BKs?|Booklets?|Fonts?|Music|OST|SoundTrack)$")
        .expect("Invalid non-video pattern")
});

/// Special directory pattern: SPs, Specials, OVA, etc.
static SPECIAL_DIR_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)^(SPs?|Specials?|特典|OVA|OAD|NCOP|NCED|PV|CM|Menu|Extras?)$")
        .expect("Invalid special pattern")
});

/// Episode number pattern for BDRip format: [26], [01], etc.
static BDRIP_EPISODE_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\[(\d{1,3})\]").expect("Invalid episode pattern"));

/// Special number pattern: SP01, OVA1, [SP01], etc.
/// Must have a SP/OVA/OAD prefix to avoid matching season numbers
static SPECIAL_NUMBER_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)(?:\[?(?:SP|OVA|OAD|特典)\s*0?(\d{1,2})\]?|\[SP(\d{1,2})\])")
        .expect("Invalid special number pattern")
});

/// BDRip file path parser
pub struct BDRipParser;

impl BDRipParser {
    /// Parse a BDRip file path to extract content type, season, and episode/special number
    ///
    /// # Arguments
    /// * `file_path` - Relative file path within the torrent
    ///
    /// # Returns
    /// `BDRipParseResult` containing the parsed information
    pub fn parse(file_path: &str) -> BDRipParseResult {
        let parts: Vec<&str> = file_path.split('/').collect();

        // Check for non-video directory (CDs, Scans, etc.)
        if parts.iter().any(|p| NON_VIDEO_DIR_PATTERN.is_match(p)) {
            return BDRipParseResult {
                content_type: BDRipContentType::NonVideo,
                season: None,
                number: None,
                original_path: file_path.to_string(),
            };
        }

        // Check for special directory (SPs, Specials, etc.)
        let is_special = parts.iter().any(|p| SPECIAL_DIR_PATTERN.is_match(p));

        // Extract season from directory names
        let season = parts.iter().find_map(|p| Self::parse_season_from_dir(p));

        // Get filename for number extraction
        let filename = parts.last().unwrap_or(&"");

        // Extract episode or special number
        let number = if is_special {
            Self::parse_special_number(filename)
        } else {
            Self::parse_episode_number(filename)
        };

        BDRipParseResult {
            content_type: if is_special {
                BDRipContentType::Special
            } else {
                BDRipContentType::Episode
            },
            season,
            number,
            original_path: file_path.to_string(),
        }
    }

    /// Parse season number from directory name
    ///
    /// Supports formats:
    /// - "2nd Season" -> 2
    /// - "Season 2" -> 2
    /// - "第二季" -> 2
    fn parse_season_from_dir(dir_name: &str) -> Option<i32> {
        let caps = SEASON_DIR_PATTERN.captures(dir_name)?;

        // Try each capture group
        for i in 1..=3 {
            if let Some(m) = caps.get(i) {
                let s = m.as_str();

                // Try parsing as number
                if let Ok(n) = s.parse::<i32>() {
                    return Some(n);
                }

                // Try Chinese number mapping
                if let Some(&n) = CHINESE_NUMBER_MAP.get(s) {
                    return Some(n);
                }
            }
        }

        None
    }

    /// Parse episode number from BDRip filename
    ///
    /// Supports format: [26], [01], etc.
    /// Falls back to general parser for other formats like EP01, 第01话
    fn parse_episode_number(filename: &str) -> Option<i32> {
        // First try BDRip-specific format: [26]
        if let Some(ep) = BDRIP_EPISODE_PATTERN
            .captures(filename)
            .and_then(|c| c.get(1))
            .and_then(|m| m.as_str().parse().ok())
        {
            return Some(ep);
        }

        // Fallback to general parser for formats like EP01, 第01话, etc.
        crate::Parser::new()
            .parse(filename)
            .ok()
            .and_then(|r| r.episode)
    }

    /// Parse special number from filename
    ///
    /// Supports formats: SP01, OVA1, 特典01, [SP01], etc.
    fn parse_special_number(filename: &str) -> Option<i32> {
        SPECIAL_NUMBER_PATTERN.captures(filename).and_then(|c| {
            // Try first capture group, then second
            c.get(1)
                .or_else(|| c.get(2))
                .and_then(|m| m.as_str().parse().ok())
        })
    }

    /// Check if a directory name indicates non-video content
    pub fn is_non_video_dir(dir_name: &str) -> bool {
        NON_VIDEO_DIR_PATTERN.is_match(dir_name)
    }

    /// Check if a directory name indicates special content
    pub fn is_special_dir(dir_name: &str) -> bool {
        SPECIAL_DIR_PATTERN.is_match(dir_name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test VCB-Studio typical directory structure:
    /// ```
    /// [VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu
    /// └── [VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [Ma10p_1080p]
    ///     ├── CDs/
    ///     ├── SPs/
    ///     │   └── [VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [SP01][Ma10p_1080p][x265_flac_aac].mkv
    ///     └── [VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [26][Ma10p_1080p][x265_flac_aac].mkv
    /// ```
    mod vcb_studio_structure {
        use super::*;

        const BASE_DIR: &str = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu";
        const SEASON_DIR: &str =
            "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [Ma10p_1080p]";

        #[test]
        fn test_vcb_episode_parsing() {
            let path = format!(
                "{}/{}/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [26][Ma10p_1080p][x265_flac_aac].mkv",
                BASE_DIR, SEASON_DIR
            );

            let result = BDRipParser::parse(&path);

            assert_eq!(result.content_type, BDRipContentType::Episode);
            assert_eq!(
                result.season,
                Some(2),
                "Should extract season 2 from '2nd Season'"
            );
            assert_eq!(
                result.number,
                Some(26),
                "Should extract episode 26 from [26]"
            );
        }

        #[test]
        fn test_vcb_special_in_sps_dir() {
            let path = format!(
                "{}/{}/SPs/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [SP01][Ma10p_1080p][x265_flac_aac].mkv",
                BASE_DIR, SEASON_DIR
            );

            let result = BDRipParser::parse(&path);

            assert_eq!(result.content_type, BDRipContentType::Special);
            assert_eq!(
                result.season,
                Some(2),
                "Should extract season 2 from '2nd Season'"
            );
            assert_eq!(
                result.number,
                Some(1),
                "Should extract SP number 1 from [SP01]"
            );
        }

        #[test]
        fn test_vcb_cds_non_video() {
            let path = format!(
                "{}/{}/CDs/[VCB-Studio] Re Zero OST [FLAC].flac",
                BASE_DIR, SEASON_DIR
            );

            let result = BDRipParser::parse(&path);

            assert_eq!(result.content_type, BDRipContentType::NonVideo);
        }

        #[test]
        fn test_vcb_first_episode() {
            let path = format!(
                "{}/{}/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [01][Ma10p_1080p][x265_flac_aac].mkv",
                BASE_DIR, SEASON_DIR
            );

            let result = BDRipParser::parse(&path);

            assert_eq!(result.content_type, BDRipContentType::Episode);
            assert_eq!(result.season, Some(2));
            assert_eq!(result.number, Some(1));
        }

        #[test]
        fn test_vcb_season_1_no_season_marker() {
            // First season typically doesn't have "1st Season" in the name
            let path = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu [Ma10p_1080p]/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu [12][Ma10p_1080p][x265_flac_aac].mkv";

            let result = BDRipParser::parse(path);

            assert_eq!(result.content_type, BDRipContentType::Episode);
            assert_eq!(
                result.season, None,
                "Season 1 typically has no marker, should be None"
            );
            assert_eq!(result.number, Some(12));
        }
    }
}
