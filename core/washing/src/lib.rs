//! Priority-based torrent washing algorithm library.
//!
//! This crate provides a priority-based system for selecting the best torrent
//! among multiple options for the same episode. Priority is determined by:
//! 1. Subtitle group (highest weight)
//! 2. Subtitle language combination (exact match)
//!
//! # Example
//!
//! ```
//! use washing::{PriorityConfig, PriorityCalculator, ComparableTorrent, SubtitleLanguageSet};
//! use parser::SubType;
//!
//! let config = PriorityConfig {
//!     subtitle_groups: vec!["ANi".to_string(), "LoliHouse".to_string()],
//!     subtitle_language_sets: vec![
//!         SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Jpn]),
//!         SubtitleLanguageSet::new(vec![SubType::Cht]),
//!     ],
//! };
//!
//! let calculator = PriorityCalculator::new(config);
//!
//! let torrent = ComparableTorrent {
//!     subtitle_group: Some("ANi".to_string()),
//!     subtitle_languages: vec![SubType::Chs, SubType::Jpn],
//! };
//!
//! let score = calculator.calculate_score(&torrent);
//! assert_eq!(score.group_rank, 0); // Highest priority
//! assert_eq!(score.language_rank, 0); // Exact match first combination
//! ```

use std::cmp::Ordering;

use serde::{Deserialize, Serialize};

pub use parser::SubType;

/// A normalized set of subtitle languages for exact matching.
///
/// Languages are automatically sorted and deduplicated on creation,
/// ensuring that `[Chs, Jpn]` and `[Jpn, Chs]` are treated as equal.
///
/// Serializes as a simple array of SubType (e.g., `["CHS", "JPN"]`).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SubtitleLanguageSet(Vec<SubType>);

impl SubtitleLanguageSet {
    /// Create a new language set from a list of languages.
    /// Automatically sorts and deduplicates the languages.
    pub fn new(mut languages: Vec<SubType>) -> Self {
        languages.sort();
        languages.dedup();
        Self(languages)
    }

    /// Check if this set exactly matches the given languages (ignoring order).
    pub fn exact_match(&self, other: &[SubType]) -> bool {
        if self.0.len() != other.len() {
            return false;
        }

        let mut other_sorted = other.to_vec();
        other_sorted.sort();
        other_sorted.dedup();

        self.0 == other_sorted
    }

    /// Get the languages in this set.
    pub fn languages(&self) -> &[SubType] {
        &self.0
    }

    /// Check if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<SubType>> for SubtitleLanguageSet {
    fn from(languages: Vec<SubType>) -> Self {
        Self::new(languages)
    }
}

impl std::fmt::Display for SubtitleLanguageSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.0
                .iter()
                .map(|l| l.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// Priority configuration from settings
#[derive(Debug, Clone, Default)]
pub struct PriorityConfig {
    /// Subtitle groups in priority order (first = highest priority)
    pub subtitle_groups: Vec<String>,
    /// Subtitle language combinations in priority order (first = highest priority)
    /// Each entry is a set of languages that must exactly match
    pub subtitle_language_sets: Vec<SubtitleLanguageSet>,
}

/// Priority score for comparison (lower rank = higher priority)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PriorityScore {
    /// Subtitle group rank (0 = highest, usize::MAX = not configured/unknown)
    pub group_rank: usize,
    /// Subtitle language combination rank
    pub language_rank: usize,
}

impl PriorityScore {
    /// Create a score with all ranks set to lowest priority
    pub fn lowest() -> Self {
        Self {
            group_rank: usize::MAX,
            language_rank: usize::MAX,
        }
    }

    /// Check if this score has any configured priority (not all MAX)
    pub fn has_any_priority(&self) -> bool {
        self.group_rank != usize::MAX || self.language_rank != usize::MAX
    }
}

impl Ord for PriorityScore {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare in order: group > language
        // Lower rank = higher priority, so we compare in reverse
        match self.group_rank.cmp(&other.group_rank) {
            Ordering::Equal => self.language_rank.cmp(&other.language_rank),
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
    /// Subtitle languages/types (e.g., [Chs, Jpn])
    pub subtitle_languages: Vec<SubType>,
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
            group_rank: self.get_group_rank(&torrent.subtitle_group),
            language_rank: self.get_language_rank(&torrent.subtitle_languages),
        }
    }

    /// Get exact match rank for subtitle group
    fn get_group_rank(&self, value: &Option<String>) -> usize {
        match value {
            Some(v) => self
                .config
                .subtitle_groups
                .iter()
                .position(|configured| configured == v)
                .unwrap_or(usize::MAX),
            None => usize::MAX,
        }
    }

    /// Get exact match rank for subtitle language combination.
    /// Returns the index of the first matching language set, or usize::MAX if no match.
    fn get_language_rank(&self, languages: &[SubType]) -> usize {
        if languages.is_empty() {
            return usize::MAX;
        }

        // Find exact match in configured language sets
        self.config
            .subtitle_language_sets
            .iter()
            .position(|set| set.exact_match(languages))
            .unwrap_or(usize::MAX)
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
            subtitle_language_sets: vec![
                SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Jpn]),
                SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Cht, SubType::Jpn]),
                SubtitleLanguageSet::new(vec![SubType::Cht]),
            ],
        }
    }

    #[test]
    fn test_subtitle_language_set_normalization() {
        // Different order should result in same set
        let set1 = SubtitleLanguageSet::new(vec![SubType::Jpn, SubType::Chs]);
        let set2 = SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Jpn]);
        assert_eq!(set1, set2);

        // Duplicates should be removed
        let set3 = SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Chs, SubType::Jpn]);
        assert_eq!(set1, set3);
    }

    #[test]
    fn test_subtitle_language_set_exact_match() {
        let set = SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Jpn]);

        // Exact match with different order
        assert!(set.exact_match(&[SubType::Jpn, SubType::Chs]));
        assert!(set.exact_match(&[SubType::Chs, SubType::Jpn]));

        // Not a match - missing language
        assert!(!set.exact_match(&[SubType::Chs]));

        // Not a match - extra language
        assert!(!set.exact_match(&[SubType::Chs, SubType::Jpn, SubType::Cht]));

        // Not a match - different languages
        assert!(!set.exact_match(&[SubType::Cht, SubType::Jpn]));
    }

    #[test]
    fn test_priority_score_ordering() {
        let score1 = PriorityScore {
            group_rank: 0,
            language_rank: 0,
        };
        let score2 = PriorityScore {
            group_rank: 1,
            language_rank: 0,
        };
        let score3 = PriorityScore {
            group_rank: 0,
            language_rank: 1,
        };

        // Lower rank = higher priority = smaller in ordering
        assert!(score1 < score2);
        assert!(score1 < score3);
        assert!(score3 < score2); // group_rank takes precedence
    }

    #[test]
    fn test_calculate_score_exact_match() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        // Exact match: [Chs, Jpn] matches first language set
        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Chs, SubType::Jpn],
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.group_rank, 0);
        assert_eq!(score.language_rank, 0);
    }

    #[test]
    fn test_calculate_score_reversed_order() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        // Different order should still match
        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Jpn, SubType::Chs], // Reversed
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.language_rank, 0); // Should still be first match
    }

    #[test]
    fn test_calculate_score_no_match() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        // [Chs] alone is not in the config
        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Chs],
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.language_rank, usize::MAX);
    }

    #[test]
    fn test_chs_jpn_better_than_chs_cht_jpn() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        // 简日 [Chs, Jpn]
        let torrent_chs_jpn = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Chs, SubType::Jpn],
        };

        // 简繁日 [Chs, Cht, Jpn]
        let torrent_chs_cht_jpn = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Chs, SubType::Cht, SubType::Jpn],
        };

        let score_chs_jpn = calculator.calculate_score(&torrent_chs_jpn);
        let score_chs_cht_jpn = calculator.calculate_score(&torrent_chs_cht_jpn);

        // 简日 should have lower rank (higher priority)
        assert_eq!(score_chs_jpn.language_rank, 0);
        assert_eq!(score_chs_cht_jpn.language_rank, 1);
        assert!(score_chs_jpn < score_chs_cht_jpn);

        // is_higher_priority should confirm this
        assert!(calculator.is_higher_priority(&torrent_chs_jpn, &torrent_chs_cht_jpn));
        assert!(!calculator.is_higher_priority(&torrent_chs_cht_jpn, &torrent_chs_jpn));
    }

    #[test]
    fn test_unknown_values_get_max_rank() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent = ComparableTorrent {
            subtitle_group: Some("未知字幕组".to_string()),
            subtitle_languages: vec![],
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.group_rank, usize::MAX);
        assert_eq!(score.language_rank, usize::MAX);
    }

    #[test]
    fn test_is_higher_priority() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent_ani = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Chs, SubType::Jpn],
        };

        let torrent_other = ComparableTorrent {
            subtitle_group: Some("喵萌奶茶屋".to_string()),
            subtitle_languages: vec![SubType::Chs, SubType::Jpn],
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
                subtitle_languages: vec![SubType::Cht],
            },
            ComparableTorrent {
                subtitle_group: Some("ANi".to_string()),
                subtitle_languages: vec![SubType::Chs, SubType::Jpn],
            },
            ComparableTorrent {
                subtitle_group: Some("喵萌奶茶屋".to_string()),
                subtitle_languages: vec![SubType::Chs, SubType::Cht, SubType::Jpn],
            },
        ];

        let best = calculator.find_best(&torrents).unwrap();
        // ANi should be best because group has highest weight
        assert_eq!(best.subtitle_group, Some("ANi".to_string()));
    }

    #[test]
    fn test_unconfigured_combination_is_lowest() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        // [Chs, Eng] is not configured
        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Chs, SubType::Eng],
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.language_rank, usize::MAX);
    }
}
