//! Priority calculation service for torrent comparison.
//!
//! This service determines which torrent has higher priority based on
//! subtitle group and language preferences.

use std::cmp::Ordering;

use super::{ComparableTorrent, SubtitleType};

/// A normalized set of subtitle languages for exact matching.
///
/// Languages are automatically sorted and deduplicated on creation,
/// ensuring that `[Chs, Jpn]` and `[Jpn, Chs]` are treated as equal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SubtitleLanguageSet(Vec<SubtitleType>);

impl SubtitleLanguageSet {
    /// Create a new language set from a list of languages.
    /// Automatically sorts and deduplicates the languages.
    pub fn new(mut languages: Vec<SubtitleType>) -> Self {
        languages.sort_by_key(|l| l.as_str());
        languages.dedup();
        Self(languages)
    }

    /// Check if this set exactly matches the given languages (ignoring order).
    pub fn exact_match(&self, other: &[SubtitleType]) -> bool {
        if self.0.len() != other.len() {
            return false;
        }

        let mut other_sorted = other.to_vec();
        other_sorted.sort_by_key(|l| l.as_str());
        other_sorted.dedup();

        self.0.iter().zip(other_sorted.iter()).all(|(a, b)| a.as_str() == b.as_str())
    }

    /// Get the languages in this set.
    pub fn languages(&self) -> &[SubtitleType] {
        &self.0
    }

    /// Check if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<SubtitleType>> for SubtitleLanguageSet {
    fn from(languages: Vec<SubtitleType>) -> Self {
        Self::new(languages)
    }
}

/// Priority configuration for the calculator.
#[derive(Debug, Clone, Default)]
pub struct PriorityConfig {
    /// Subtitle groups in priority order (first = highest priority).
    pub subtitle_groups: Vec<String>,
    /// Subtitle language combinations in priority order (first = highest priority).
    /// Each entry is a set of languages that must exactly match.
    pub subtitle_language_sets: Vec<SubtitleLanguageSet>,
}

/// Priority score for comparison (lower rank = higher priority).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PriorityScore {
    /// Subtitle group rank (0 = highest, usize::MAX = not configured/unknown).
    pub group_rank: usize,
    /// Subtitle language combination rank.
    pub language_rank: usize,
}

impl PriorityScore {
    /// Create a score with all ranks set to lowest priority.
    pub fn lowest() -> Self {
        Self {
            group_rank: usize::MAX,
            language_rank: usize::MAX,
        }
    }

    /// Check if this score has any configured priority (not all MAX).
    pub fn has_any_priority(&self) -> bool {
        self.group_rank != usize::MAX || self.language_rank != usize::MAX
    }
}

impl Ord for PriorityScore {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare in order: group > language
        // Lower rank = higher priority, so we compare directly
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

/// Priority calculator for comparing torrents.
///
/// Determines which torrent should be preferred based on subtitle group
/// and language preferences defined in the configuration.
pub struct PriorityCalculator {
    config: PriorityConfig,
}

impl PriorityCalculator {
    /// Create a new priority calculator with the given configuration.
    pub fn new(config: PriorityConfig) -> Self {
        Self { config }
    }

    /// Calculate priority score for a torrent.
    pub fn calculate_score(&self, torrent: &ComparableTorrent) -> PriorityScore {
        PriorityScore {
            group_rank: self.get_group_rank(&torrent.subtitle_group),
            language_rank: self.get_language_rank(&torrent.subtitle_languages),
        }
    }

    /// Get exact match rank for subtitle group.
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
    fn get_language_rank(&self, languages: &[SubtitleType]) -> usize {
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

    /// Check if new torrent has higher priority than existing.
    ///
    /// Returns true if `new` should replace `existing`.
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

    /// Find the torrent with highest priority from a list.
    ///
    /// Returns the torrent with the best (lowest) priority score.
    pub fn find_best<'a>(&self, torrents: &'a [ComparableTorrent]) -> Option<&'a ComparableTorrent> {
        torrents.iter().min_by_key(|t| self.calculate_score(t))
    }

    /// Find the best torrent and its score from a list.
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
                "LoliHouse".to_string(),
            ],
            subtitle_language_sets: vec![
                SubtitleLanguageSet::new(vec![SubtitleType::SimplifiedChinese, SubtitleType::Japanese]),
                SubtitleLanguageSet::new(vec![SubtitleType::TraditionalChinese]),
            ],
        }
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

        assert!(score1 < score2);
    }

    #[test]
    fn test_is_higher_priority() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent_ani = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubtitleType::SimplifiedChinese, SubtitleType::Japanese],
        };

        let torrent_other = ComparableTorrent {
            subtitle_group: Some("LoliHouse".to_string()),
            subtitle_languages: vec![SubtitleType::SimplifiedChinese, SubtitleType::Japanese],
        };

        assert!(calculator.is_higher_priority(&torrent_ani, &torrent_other));
        assert!(!calculator.is_higher_priority(&torrent_other, &torrent_ani));
    }
}
