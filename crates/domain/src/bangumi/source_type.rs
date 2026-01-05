//! Source type value object.
//!
//! Represents the source type for a bangumi (WebRip or BDRip).

use serde::{Deserialize, Serialize};
use std::str::FromStr;

/// Source type for bangumi.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SourceType {
    /// Web release (streaming rip).
    #[default]
    WebRip,
    /// Blu-ray release.
    BDRip,
}

impl SourceType {
    /// Get the string representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            SourceType::WebRip => "webrip",
            SourceType::BDRip => "bdrip",
        }
    }

    /// Check if this is a WebRip source.
    pub fn is_webrip(&self) -> bool {
        matches!(self, SourceType::WebRip)
    }

    /// Check if this is a BDRip source.
    pub fn is_bdrip(&self) -> bool {
        matches!(self, SourceType::BDRip)
    }
}

impl FromStr for SourceType {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "bdrip" => SourceType::BDRip,
            _ => SourceType::WebRip,
        })
    }
}

impl std::fmt::Display for SourceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
