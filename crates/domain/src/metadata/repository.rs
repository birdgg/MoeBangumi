//! Metadata repository trait.
//!
//! Defines the abstract interface for Metadata persistence operations.

use async_trait::async_trait;

use super::Metadata;
use crate::error::DomainResult;

/// Metadata repository trait.
///
/// Defines the interface for Metadata persistence operations.
/// Concrete implementations are provided in the infrastructure layer.
#[async_trait]
pub trait MetadataRepository: Send + Sync {
    /// Find a Metadata by its ID.
    async fn find_by_id(&self, id: i64) -> DomainResult<Option<Metadata>>;

    /// Find a Metadata by its BGM.tv ID.
    async fn find_by_bgmtv_id(&self, bgmtv_id: i64) -> DomainResult<Option<Metadata>>;

    /// Find a Metadata by its Mikan ID.
    async fn find_by_mikan_id(&self, mikan_id: &str) -> DomainResult<Option<Metadata>>;

    /// Find all Metadata entries.
    async fn find_all(&self) -> DomainResult<Vec<Metadata>>;

    /// Find Metadata entries that need TMDB lookup.
    ///
    /// Returns Metadata where:
    /// - TMDB ID is not set, AND
    /// - Either never looked up, or last lookup was more than specified days ago
    async fn find_needing_tmdb_lookup(&self, days_threshold: i64) -> DomainResult<Vec<Metadata>>;

    /// Save a Metadata (create or update).
    ///
    /// If the Metadata has a new ID (0), it will be created.
    /// Otherwise, it will be updated.
    async fn save(&self, metadata: &mut Metadata) -> DomainResult<()>;

    /// Delete a Metadata by its ID.
    ///
    /// Returns true if the Metadata was deleted, false if not found.
    async fn delete(&self, id: i64) -> DomainResult<bool>;
}
