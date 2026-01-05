//! Metadata entity.
//!
//! Represents anime metadata from external sources (BGM.tv, TMDB, Mikan).

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::{CreateMetadataRequest, EpisodeOffset, Platform, Season};

/// Metadata entity for anime information.
///
/// Unified metadata center caching data from BGM.tv, TMDB, and Mikan.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,

    /// Mikan bangumi ID.
    pub mikan_id: Option<String>,
    /// BGM.tv subject ID.
    pub bgmtv_id: Option<i64>,
    /// TMDB ID.
    pub tmdb_id: Option<i64>,

    /// Chinese title (primary display).
    pub title_chinese: String,
    /// Japanese original name.
    pub title_japanese: Option<String>,

    /// Season number.
    pub season: Season,
    /// Year.
    pub year: i32,
    /// Platform type (TV, Movie, OVA).
    pub platform: Platform,
    /// Episode offset for converting RSS episode numbers to season-relative numbers.
    pub episode_offset: EpisodeOffset,

    /// Total episodes (0 = unknown).
    pub total_episodes: i32,
    /// Poster image URL.
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD format).
    pub air_date: Option<String>,
    /// Day of week when new episodes air (0=Sunday ~ 6=Saturday).
    pub air_week: i32,

    /// Last TMDB lookup attempt timestamp.
    pub tmdb_lookup_at: Option<DateTime<Utc>>,
}

impl Metadata {
    /// Create a new Metadata from a validated request.
    pub fn create(request: CreateMetadataRequest) -> Self {
        let now = Utc::now();
        Self {
            id: 0,
            created_at: now,
            updated_at: now,
            mikan_id: request.mikan_id,
            bgmtv_id: request.bgmtv_id,
            tmdb_id: None,
            title_chinese: request.title_chinese,
            title_japanese: request.title_japanese,
            season: request.season,
            year: request.year,
            platform: request.platform,
            episode_offset: request.episode_offset,
            total_episodes: 0,
            poster_url: None,
            air_date: None,
            air_week: 0,
            tmdb_lookup_at: None,
        }
    }

    /// Reconstitute a Metadata from persistence (used by repository).
    #[allow(clippy::too_many_arguments)]
    pub fn reconstitute(
        id: i64,
        created_at: DateTime<Utc>,
        updated_at: DateTime<Utc>,
        mikan_id: Option<String>,
        bgmtv_id: Option<i64>,
        tmdb_id: Option<i64>,
        title_chinese: String,
        title_japanese: Option<String>,
        season: Season,
        year: i32,
        platform: Platform,
        episode_offset: EpisodeOffset,
        total_episodes: i32,
        poster_url: Option<String>,
        air_date: Option<String>,
        air_week: i32,
        tmdb_lookup_at: Option<DateTime<Utc>>,
    ) -> Self {
        Self {
            id,
            created_at,
            updated_at,
            mikan_id,
            bgmtv_id,
            tmdb_id,
            title_chinese,
            title_japanese,
            season,
            year,
            platform,
            episode_offset,
            total_episodes,
            poster_url,
            air_date,
            air_week,
            tmdb_lookup_at,
        }
    }

    // === Business Logic ===

    /// Convert RSS episode number to season-relative episode number.
    ///
    /// RSS feeds typically use absolute episode numbers (e.g., episode 13 for a split-cour anime),
    /// but media servers expect season-relative numbers (e.g., S02E01).
    #[inline]
    pub fn adjust_episode(&self, episode: i32) -> i32 {
        self.episode_offset.apply(episode)
    }

    /// Check if TMDB lookup should be attempted.
    ///
    /// Returns true if:
    /// - TMDB ID is not set, AND
    /// - Either never looked up, or last lookup was more than 7 days ago
    pub fn should_lookup_tmdb(&self) -> bool {
        if self.tmdb_id.is_some() {
            return false;
        }

        match self.tmdb_lookup_at {
            None => true,
            Some(last_lookup) => {
                let days_since = Utc::now().signed_duration_since(last_lookup).num_days();
                days_since >= 7
            }
        }
    }

    /// Check if this metadata has a poster.
    pub fn has_poster(&self) -> bool {
        self.poster_url.is_some()
    }

    /// Get the display title (prefers Chinese title).
    pub fn display_title(&self) -> &str {
        &self.title_chinese
    }
}
