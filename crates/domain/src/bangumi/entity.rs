//! Bangumi entity.
//!
//! Represents a user's subscription to an anime series, including
//! download configuration and progress tracking.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::{CreateBangumiRequest, SourceType};

/// Bangumi (anime) subscription entity.
///
/// Stores user's subscription state and download configuration.
/// This is the core aggregate root for anime tracking.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bangumi {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,

    /// Reference to metadata (foreign key).
    pub metadata_id: i64,

    /// Current downloaded episode.
    pub current_episode: i32,

    /// Only download latest episode.
    pub auto_complete: bool,

    /// Save path for downloads.
    pub save_path: String,

    /// Source type: webrip or bdrip.
    pub source_type: SourceType,
}

impl Bangumi {
    /// Create a new Bangumi from a validated request.
    pub fn create(request: CreateBangumiRequest) -> Self {
        let now = Utc::now();
        Self {
            id: 0,
            created_at: now,
            updated_at: now,
            metadata_id: request.metadata_id,
            current_episode: 0,
            auto_complete: true,
            save_path: request.save_path,
            source_type: request.source_type,
        }
    }

    /// Reconstitute a Bangumi from persistence (used by repository).
    pub fn reconstitute(
        id: i64,
        created_at: DateTime<Utc>,
        updated_at: DateTime<Utc>,
        metadata_id: i64,
        current_episode: i32,
        auto_complete: bool,
        save_path: String,
        source_type: SourceType,
    ) -> Self {
        Self {
            id,
            created_at,
            updated_at,
            metadata_id,
            current_episode,
            auto_complete,
            save_path,
            source_type,
        }
    }

    /// Update the current episode if the new episode is greater.
    ///
    /// Returns true if the episode was updated.
    pub fn update_current_episode(&mut self, episode: i32) -> bool {
        if episode > self.current_episode {
            self.current_episode = episode;
            self.updated_at = Utc::now();
            true
        } else {
            false
        }
    }

    /// Check if the anime is complete based on total episodes.
    pub fn is_complete(&self, total_episodes: i32) -> bool {
        total_episodes > 0 && self.current_episode >= total_episodes
    }
}
