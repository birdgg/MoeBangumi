//! Torrent repository trait.
//!
//! Defines the abstract interface for Torrent persistence operations.

use async_trait::async_trait;

use super::Torrent;
use crate::error::DomainResult;

/// Torrent repository trait.
///
/// Defines the interface for Torrent persistence operations.
/// Concrete implementations are provided in the infrastructure layer.
#[async_trait]
pub trait TorrentRepository: Send + Sync {
    /// Find a Torrent by its ID.
    async fn find_by_id(&self, id: i64) -> DomainResult<Option<Torrent>>;

    /// Find a Torrent by its info hash.
    async fn find_by_info_hash(&self, info_hash: &str) -> DomainResult<Option<Torrent>>;

    /// Find all Torrents for a specific Bangumi.
    async fn find_by_bangumi_id(&self, bangumi_id: i64) -> DomainResult<Vec<Torrent>>;

    /// Find all Torrents for a specific RSS subscription.
    async fn find_by_rss_id(&self, rss_id: i64) -> DomainResult<Vec<Torrent>>;

    /// Find all Torrents for a specific Bangumi and episode.
    async fn find_by_bangumi_and_episode(
        &self,
        bangumi_id: i64,
        episode: i32,
    ) -> DomainResult<Vec<Torrent>>;

    /// Find all Torrents.
    async fn find_all(&self) -> DomainResult<Vec<Torrent>>;

    /// Save a Torrent (create or update).
    ///
    /// If the Torrent has a new ID (0), it will be created.
    /// Otherwise, it will be updated.
    async fn save(&self, torrent: &mut Torrent) -> DomainResult<()>;

    /// Delete a Torrent by its ID.
    ///
    /// Returns true if the Torrent was deleted, false if not found.
    async fn delete(&self, id: i64) -> DomainResult<bool>;

    /// Delete a Torrent by its info hash.
    ///
    /// Returns true if the Torrent was deleted, false if not found.
    async fn delete_by_info_hash(&self, info_hash: &str) -> DomainResult<bool>;

    /// Delete all Torrents for a specific Bangumi.
    ///
    /// Returns the number of deleted torrents.
    async fn delete_by_bangumi_id(&self, bangumi_id: i64) -> DomainResult<u64>;

    /// Check if a Torrent with the given info hash exists.
    async fn exists_by_info_hash(&self, info_hash: &str) -> DomainResult<bool>;
}
