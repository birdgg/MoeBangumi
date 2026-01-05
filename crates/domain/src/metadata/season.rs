//! Season value object with validation.

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// A validated anime season number.
///
/// Season numbers must be positive integers (1 or greater).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Season(i32);

/// Error when season number is invalid.
#[derive(Clone, Debug, Error)]
pub enum SeasonError {
    /// Season number must be positive.
    #[error("Season number must be positive, got {0}")]
    NotPositive(i32),
}

impl Season {
    /// Create a new Season with validation.
    ///
    /// # Errors
    ///
    /// Returns `SeasonError::NotPositive` if the season number is less than 1.
    pub fn new(value: i32) -> Result<Self, SeasonError> {
        if value < 1 {
            Err(SeasonError::NotPositive(value))
        } else {
            Ok(Self(value))
        }
    }

    /// Get the season number as i32.
    pub fn as_i32(&self) -> i32 {
        self.0
    }

    /// Create Season 1 (the most common default).
    pub fn one() -> Self {
        Self(1)
    }
}

impl std::fmt::Display for Season {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "S{:02}", self.0)
    }
}

impl Default for Season {
    fn default() -> Self {
        Self::one()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_season() {
        assert!(Season::new(1).is_ok());
        assert!(Season::new(10).is_ok());
        assert_eq!(Season::new(5).unwrap().as_i32(), 5);
    }

    #[test]
    fn test_invalid_season() {
        assert!(Season::new(0).is_err());
        assert!(Season::new(-1).is_err());
    }

    #[test]
    fn test_display() {
        assert_eq!(Season::new(1).unwrap().to_string(), "S01");
        assert_eq!(Season::new(12).unwrap().to_string(), "S12");
    }
}
