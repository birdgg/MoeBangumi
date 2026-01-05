//! Platform type value object.
//!
//! Represents the platform/format type for a bangumi (TV, Movie, OVA).

use serde::{Deserialize, Serialize};
use std::str::FromStr;

/// Platform type for bangumi (TV, Movie, OVA).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    /// TV series.
    #[default]
    Tv,
    /// Movie.
    Movie,
    /// OVA (Original Video Animation).
    Ova,
}

impl Platform {
    /// Get the string representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            Platform::Tv => "tv",
            Platform::Movie => "movie",
            Platform::Ova => "ova",
        }
    }

    /// Check if this platform is a movie.
    pub fn is_movie(&self) -> bool {
        matches!(self, Platform::Movie)
    }

    /// Check if this platform is a TV series.
    pub fn is_tv(&self) -> bool {
        matches!(self, Platform::Tv)
    }

    /// Check if this platform is an OVA.
    pub fn is_ova(&self) -> bool {
        matches!(self, Platform::Ova)
    }
}

impl FromStr for Platform {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "movie" => Platform::Movie,
            "ova" => Platform::Ova,
            _ => Platform::Tv,
        })
    }
}

impl std::fmt::Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
