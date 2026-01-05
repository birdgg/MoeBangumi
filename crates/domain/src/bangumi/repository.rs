//! Bangumi repository trait.
//!
//! Defines the abstract interface for Bangumi persistence operations.

use async_trait::async_trait;

use super::Bangumi;
use crate::error::DomainResult;

/// Bangumi repository trait.
///
/// Defines the interface for Bangumi persistence operations.
/// Concrete implementations are provided in the infrastructure layer.
#[async_trait]
pub trait BangumiRepository: Send + Sync {
    /// Find a Bangumi by its ID.
    async fn find_by_id(&self, id: i64) -> DomainResult<Option<Bangumi>>;

    /// Find a Bangumi by its associated metadata ID.
    async fn find_by_metadata_id(&self, metadata_id: i64) -> DomainResult<Option<Bangumi>>;

    /// Find all Bangumi entries.
    async fn find_all(&self) -> DomainResult<Vec<Bangumi>>;

    /// Save a Bangumi (create or update).
    ///
    /// If the Bangumi has a new ID (0), it will be created.
    /// Otherwise, it will be updated.
    async fn save(&self, bangumi: &mut Bangumi) -> DomainResult<()>;

    /// Delete a Bangumi by its ID.
    ///
    /// Returns true if the Bangumi was deleted, false if not found.
    async fn delete(&self, id: i64) -> DomainResult<bool>;

    /// Update the current episode for a Bangumi only if the new value is greater.
    ///
    /// This is an atomic operation to avoid race conditions.
    /// Returns true if the episode was updated, false otherwise.
    async fn update_current_episode_if_greater(
        &self,
        id: i64,
        episode: i32,
    ) -> DomainResult<bool>;
}
