//! Priority-based torrent washing algorithm library.
//!
//! This crate provides a priority-based system for selecting the best torrent
//! among multiple options for the same episode. Priority is determined by:
//! 1. Subtitle group (highest weight)
//! 2. Subtitle language
//! 3. Resolution (lowest weight)
//!
//! # Example
//!
//! ```
//! use washing::{PriorityConfig, PriorityCalculator, ComparableTorrent};
//!
//! let config = PriorityConfig {
//!     subtitle_groups: vec!["ANi".to_string(), "LoliHouse".to_string()],
//!     subtitle_languages: vec!["简日".to_string(), "简体".to_string()],
//!     resolutions: vec!["1080P".to_string(), "720P".to_string()],
//! };
//!
//! let calculator = PriorityCalculator::new(config);
//!
//! let torrent = ComparableTorrent {
//!     subtitle_group: Some("ANi".to_string()),
//!     subtitle_language: Some("简日".to_string()),
//!     resolution: Some("1080P".to_string()),
//! };
//!
//! let score = calculator.calculate_score(&torrent);
//! assert_eq!(score.group_rank, 0); // Highest priority
//! ```

use std::cmp::Ordering;

/// Priority configuration from settings
#[derive(Debug, Clone, Default)]
pub struct PriorityConfig {
    /// Subtitle groups in priority order (first = highest priority)
    pub subtitle_groups: Vec<String>,
    /// Subtitle languages in priority order (first = highest priority)
    pub subtitle_languages: Vec<String>,
    /// Resolutions in priority order (first = highest priority)
    pub resolutions: Vec<String>,
}

/// Priority score for comparison (lower rank = higher priority)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PriorityScore {
    /// Subtitle group rank (0 = highest, usize::MAX = not configured/unknown)
    pub group_rank: usize,
    /// Subtitle language rank
    pub language_rank: usize,
    /// Resolution rank
    pub resolution_rank: usize,
}

impl PriorityScore {
    /// Create a score with all ranks set to lowest priority
    pub fn lowest() -> Self {
        Self {
            group_rank: usize::MAX,
            language_rank: usize::MAX,
            resolution_rank: usize::MAX,
        }
    }

    /// Check if this score has any configured priority (not all MAX)
    pub fn has_any_priority(&self) -> bool {
        self.group_rank != usize::MAX
            || self.language_rank != usize::MAX
            || self.resolution_rank != usize::MAX
    }
}

impl Ord for PriorityScore {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare in order: group > language > resolution
        // Lower rank = higher priority, so we compare in reverse
        match self.group_rank.cmp(&other.group_rank) {
            Ordering::Equal => match self.language_rank.cmp(&other.language_rank) {
                Ordering::Equal => self.resolution_rank.cmp(&other.resolution_rank),
                other => other,
            },
            other => other,
        }
    }
}

impl PartialOrd for PriorityScore {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Comparable torrent information extracted from parse result
#[derive(Debug, Clone, Default)]
pub struct ComparableTorrent {
    /// Subtitle group name
    pub subtitle_group: Option<String>,
    /// Subtitle language/type (e.g., "简日", "繁体")
    pub subtitle_language: Option<String>,
    /// Video resolution (e.g., "1080P", "720P")
    pub resolution: Option<String>,
}

/// Priority calculator for comparing torrents
pub struct PriorityCalculator {
    config: PriorityConfig,
}

impl PriorityCalculator {
    /// Create a new priority calculator with the given configuration
    pub fn new(config: PriorityConfig) -> Self {
        Self { config }
    }

    /// Calculate priority score for a torrent
    pub fn calculate_score(&self, torrent: &ComparableTorrent) -> PriorityScore {
        PriorityScore {
            group_rank: self.get_rank(&self.config.subtitle_groups, &torrent.subtitle_group),
            language_rank: self.get_rank_fuzzy(
                &self.config.subtitle_languages,
                &torrent.subtitle_language,
            ),
            resolution_rank: self.get_rank_case_insensitive(
                &self.config.resolutions,
                &torrent.resolution,
            ),
        }
    }

    /// Get exact match rank in the priority list
    fn get_rank(&self, priority_list: &[String], value: &Option<String>) -> usize {
        match value {
            Some(v) => priority_list
                .iter()
                .position(|configured| configured == v)
                .unwrap_or(usize::MAX),
            None => usize::MAX,
        }
    }

    /// Get case-insensitive rank in the priority list
    fn get_rank_case_insensitive(
        &self,
        priority_list: &[String],
        value: &Option<String>,
    ) -> usize {
        match value {
            Some(v) => priority_list
                .iter()
                .position(|configured| configured.eq_ignore_ascii_case(v))
                .unwrap_or(usize::MAX),
            None => usize::MAX,
        }
    }

    /// Get fuzzy match rank (value contains configured or configured contains value)
    fn get_rank_fuzzy(&self, priority_list: &[String], value: &Option<String>) -> usize {
        match value {
            Some(v) => priority_list
                .iter()
                .position(|configured| v.contains(configured) || configured.contains(v))
                .unwrap_or(usize::MAX),
            None => usize::MAX,
        }
    }

    /// Check if new torrent has higher priority than existing
    ///
    /// Returns true if `new` should replace `existing`
    pub fn is_higher_priority(
        &self,
        new: &ComparableTorrent,
        existing: &ComparableTorrent,
    ) -> bool {
        let new_score = self.calculate_score(new);
        let existing_score = self.calculate_score(existing);

        // Lower score = higher priority
        new_score < existing_score
    }

    /// Find the torrent with highest priority from a list
    ///
    /// Returns the torrent with the best (lowest) priority score
    pub fn find_best<'a>(&self, torrents: &'a [ComparableTorrent]) -> Option<&'a ComparableTorrent> {
        torrents
            .iter()
            .min_by_key(|t| self.calculate_score(t))
    }

    /// Find the best torrent and its score from a list
    pub fn find_best_with_score<'a>(
        &self,
        torrents: &'a [ComparableTorrent],
    ) -> Option<(&'a ComparableTorrent, PriorityScore)> {
        torrents
            .iter()
            .map(|t| (t, self.calculate_score(t)))
            .min_by(|(_, a), (_, b)| a.cmp(b))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_config() -> PriorityConfig {
        PriorityConfig {
            subtitle_groups: vec![
                "ANi".to_string(),
                "喵萌奶茶屋".to_string(),
                "桜都字幕组".to_string(),
            ],
            subtitle_languages: vec![
                "简日".to_string(),
                "简体".to_string(),
                "繁日".to_string(),
            ],
            resolutions: vec![
                "2160P".to_string(),
                "1080P".to_string(),
                "720P".to_string(),
            ],
        }
    }

    #[test]
    fn test_priority_score_ordering() {
        let score1 = PriorityScore {
            group_rank: 0,
            language_rank: 0,
            resolution_rank: 0,
        };
        let score2 = PriorityScore {
            group_rank: 1,
            language_rank: 0,
            resolution_rank: 0,
        };
        let score3 = PriorityScore {
            group_rank: 0,
            language_rank: 1,
            resolution_rank: 0,
        };

        // Lower rank = higher priority = smaller in ordering
        assert!(score1 < score2);
        assert!(score1 < score3);
        assert!(score3 < score2); // group_rank takes precedence
    }

    #[test]
    fn test_calculate_score() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_language: Some("简日".to_string()),
            resolution: Some("1080P".to_string()),
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.group_rank, 0);
        assert_eq!(score.language_rank, 0);
        assert_eq!(score.resolution_rank, 1);
    }

    #[test]
    fn test_unknown_values_get_max_rank() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent = ComparableTorrent {
            subtitle_group: Some("未知字幕组".to_string()),
            subtitle_language: None,
            resolution: Some("480P".to_string()),
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.group_rank, usize::MAX);
        assert_eq!(score.language_rank, usize::MAX);
        assert_eq!(score.resolution_rank, usize::MAX);
    }

    #[test]
    fn test_is_higher_priority() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent_ani = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_language: Some("简日".to_string()),
            resolution: Some("1080P".to_string()),
        };

        let torrent_other = ComparableTorrent {
            subtitle_group: Some("喵萌奶茶屋".to_string()),
            subtitle_language: Some("简日".to_string()),
            resolution: Some("1080P".to_string()),
        };

        // ANi has higher priority than 喵萌奶茶屋
        assert!(calculator.is_higher_priority(&torrent_ani, &torrent_other));
        assert!(!calculator.is_higher_priority(&torrent_other, &torrent_ani));
    }

    #[test]
    fn test_find_best() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrents = vec![
            ComparableTorrent {
                subtitle_group: Some("桜都字幕组".to_string()),
                subtitle_language: Some("简体".to_string()),
                resolution: Some("1080P".to_string()),
            },
            ComparableTorrent {
                subtitle_group: Some("ANi".to_string()),
                subtitle_language: Some("简日".to_string()),
                resolution: Some("720P".to_string()),
            },
            ComparableTorrent {
                subtitle_group: Some("喵萌奶茶屋".to_string()),
                subtitle_language: Some("繁日".to_string()),
                resolution: Some("2160P".to_string()),
            },
        ];

        let best = calculator.find_best(&torrents).unwrap();
        // ANi should be best because group has highest weight
        assert_eq!(best.subtitle_group, Some("ANi".to_string()));
    }

    #[test]
    fn test_fuzzy_language_matching() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        // "简繁日内封字幕" contains "繁日" (position 2 in config)
        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_language: Some("简繁日内封字幕".to_string()),
            resolution: Some("1080P".to_string()),
        };

        let score = calculator.calculate_score(&torrent);
        // Should match "繁日" at position 2 (0-indexed)
        assert_eq!(score.language_rank, 2);

        // Test with a string that actually contains "简日"
        let torrent2 = ComparableTorrent {
            subtitle_group: None,
            subtitle_language: Some("简日双语".to_string()),
            resolution: None,
        };
        let score2 = calculator.calculate_score(&torrent2);
        // Should match "简日" at position 0
        assert_eq!(score2.language_rank, 0);
    }

    #[test]
    fn test_case_insensitive_resolution() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent = ComparableTorrent {
            subtitle_group: None,
            subtitle_language: None,
            resolution: Some("1080p".to_string()), // lowercase
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.resolution_rank, 1); // Should match "1080P"
    }
}
