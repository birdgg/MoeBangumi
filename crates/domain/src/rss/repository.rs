//! RSS repository trait.
//!
//! Defines the abstract interface for RSS subscription persistence operations.

use async_trait::async_trait;

use super::Rss;
use crate::error::DomainResult;

/// RSS repository trait.
///
/// Defines the interface for RSS subscription persistence operations.
/// Concrete implementations are provided in the infrastructure layer.
#[async_trait]
pub trait RssRepository: Send + Sync {
    /// Find an RSS subscription by its ID.
    async fn find_by_id(&self, id: i64) -> DomainResult<Option<Rss>>;

    /// Find all RSS subscriptions for a specific Bangumi.
    async fn find_by_bangumi_id(&self, bangumi_id: i64) -> DomainResult<Vec<Rss>>;

    /// Find all enabled RSS subscriptions.
    async fn find_all_enabled(&self) -> DomainResult<Vec<Rss>>;

    /// Find all RSS subscriptions.
    async fn find_all(&self) -> DomainResult<Vec<Rss>>;

    /// Save an RSS subscription (create or update).
    ///
    /// If the RSS has a new ID (0), it will be created.
    /// Otherwise, it will be updated.
    async fn save(&self, rss: &mut Rss) -> DomainResult<()>;

    /// Delete an RSS subscription by its ID.
    ///
    /// Returns true if the RSS was deleted, false if not found.
    async fn delete(&self, id: i64) -> DomainResult<bool>;

    /// Delete all RSS subscriptions for a specific Bangumi.
    ///
    /// Returns the number of deleted subscriptions.
    async fn delete_by_bangumi_id(&self, bangumi_id: i64) -> DomainResult<u64>;

    /// Update RSS cache information (etag, last_modified, last_pub_date).
    async fn update_cache_info(
        &self,
        id: i64,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) -> DomainResult<()>;
}
