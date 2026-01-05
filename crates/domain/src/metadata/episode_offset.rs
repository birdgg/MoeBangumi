//! Episode offset value object.
//!
//! Represents the offset to apply when converting RSS episode numbers
//! to season-relative episode numbers.

use serde::{Deserialize, Serialize};

/// Episode offset value object (immutable).
///
/// RSS feeds typically use absolute episode numbers (e.g., episode 13 for a split-cour anime),
/// but media servers expect season-relative numbers (e.g., S02E01).
///
/// The offset is calculated as `sort - ep` from BGM.tv episode data.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct EpisodeOffset(i32);

impl EpisodeOffset {
    /// Create a new episode offset.
    pub fn new(offset: i32) -> Self {
        Self(offset)
    }

    /// Create a zero offset.
    pub fn zero() -> Self {
        Self(0)
    }

    /// Apply the offset to an episode number.
    ///
    /// Converts RSS absolute episode number to season-relative episode number.
    #[inline]
    pub fn apply(&self, episode: i32) -> i32 {
        episode - self.0
    }

    /// Get the raw offset value.
    pub fn as_i32(&self) -> i32 {
        self.0
    }
}

impl From<i32> for EpisodeOffset {
    fn from(value: i32) -> Self {
        Self(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_apply_offset() {
        let offset = EpisodeOffset::new(12);
        assert_eq!(offset.apply(13), 1); // Episode 13 -> S02E01
        assert_eq!(offset.apply(24), 12); // Episode 24 -> S02E12
    }

    #[test]
    fn test_zero_offset() {
        let offset = EpisodeOffset::zero();
        assert_eq!(offset.apply(5), 5);
    }
}
