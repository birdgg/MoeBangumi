//! Torrent entity.
//!
//! Represents a BitTorrent download task for anime episodes.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::CreateTorrentRequest;

/// Subtitle type for anime releases.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SubtitleType {
    /// Simplified Chinese subtitles.
    SimplifiedChinese,
    /// Traditional Chinese subtitles.
    TraditionalChinese,
    /// Japanese subtitles.
    Japanese,
    /// English subtitles.
    English,
    /// Unknown or other subtitles.
    Unknown,
}

impl SubtitleType {
    /// Get the string representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            SubtitleType::SimplifiedChinese => "chs",
            SubtitleType::TraditionalChinese => "cht",
            SubtitleType::Japanese => "jpn",
            SubtitleType::English => "eng",
            SubtitleType::Unknown => "unknown",
        }
    }
}

impl std::str::FromStr for SubtitleType {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "chs" | "simplified" | "简" | "简体" => SubtitleType::SimplifiedChinese,
            "cht" | "traditional" | "繁" | "繁体" => SubtitleType::TraditionalChinese,
            "jpn" | "japanese" | "日" => SubtitleType::Japanese,
            "eng" | "english" => SubtitleType::English,
            _ => SubtitleType::Unknown,
        })
    }
}

/// Torrent entity representing a BitTorrent file for bangumi episodes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Torrent {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,

    /// Foreign key to bangumi.
    pub bangumi_id: i64,
    /// Optional reference to source RSS.
    pub rss_id: Option<i64>,

    /// BitTorrent info hash (40-char hex for v1, 64-char for v2).
    pub info_hash: String,

    /// Torrent URL (.torrent file URL or magnet link).
    pub torrent_url: String,

    /// Episode number (optional, can be parsed from filename during rename).
    pub episode_number: Option<i32>,

    /// Parsed subtitle group name (for priority comparison).
    pub subtitle_group: Option<String>,

    /// Parsed subtitle languages (for priority comparison).
    pub subtitle_languages: Vec<SubtitleType>,

    /// Parsed video resolution (stored for display purposes only).
    pub resolution: Option<String>,
}

impl Torrent {
    /// Create a new Torrent from a validated request.
    pub fn create(request: CreateTorrentRequest) -> Self {
        let now = Utc::now();
        Self {
            id: 0,
            created_at: now,
            updated_at: now,
            bangumi_id: request.bangumi_id,
            rss_id: request.rss_id,
            info_hash: request.info_hash,
            torrent_url: request.torrent_url,
            episode_number: request.episode_number,
            subtitle_group: request.subtitle_group,
            subtitle_languages: request.subtitle_languages,
            resolution: request.resolution,
        }
    }

    /// Reconstitute a Torrent from persistence (used by repository).
    #[allow(clippy::too_many_arguments)]
    pub fn reconstitute(
        id: i64,
        created_at: DateTime<Utc>,
        updated_at: DateTime<Utc>,
        bangumi_id: i64,
        rss_id: Option<i64>,
        info_hash: String,
        torrent_url: String,
        episode_number: Option<i32>,
        subtitle_group: Option<String>,
        subtitle_languages: Vec<SubtitleType>,
        resolution: Option<String>,
    ) -> Self {
        Self {
            id,
            created_at,
            updated_at,
            bangumi_id,
            rss_id,
            info_hash,
            torrent_url,
            episode_number,
            subtitle_group,
            subtitle_languages,
            resolution,
        }
    }
}

/// Comparable torrent information for priority comparison.
///
/// Used by the priority calculator to compare torrents and determine
/// which one has higher priority based on subtitle group and languages.
#[derive(Debug, Clone)]
pub struct ComparableTorrent {
    /// Subtitle group name.
    pub subtitle_group: Option<String>,
    /// Subtitle languages.
    pub subtitle_languages: Vec<SubtitleType>,
}

impl From<&Torrent> for ComparableTorrent {
    fn from(torrent: &Torrent) -> Self {
        Self {
            subtitle_group: torrent.subtitle_group.clone(),
            subtitle_languages: torrent.subtitle_languages.clone(),
        }
    }
}
