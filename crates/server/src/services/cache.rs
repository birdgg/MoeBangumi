use serde::{de::DeserializeOwned, Serialize};
use sqlx::SqlitePool;
use std::future::Future;
use thiserror::Error;

use crate::repositories::CacheRepository;

#[derive(Debug, Error)]
pub enum CacheError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
}

/// Service for caching data with automatic fetch-on-miss
pub struct CacheService {
    db: SqlitePool,
}

impl CacheService {
    /// Create a new CacheService
    pub fn new(db: SqlitePool) -> Self {
        Self { db }
    }

    /// Get cached data or fetch it using the provided async function
    ///
    /// This method implements the cache-aside pattern:
    /// 1. Try to get data from cache
    /// 2. If cache miss or expired, call the fetcher function
    /// 3. Store the result in cache
    /// 4. Return the data
    pub async fn get_or_fetch<T, F, Fut, E>(
        &self,
        key: &str,
        max_age_secs: i64,
        fetcher: F,
    ) -> Result<T, E>
    where
        T: Serialize + DeserializeOwned,
        F: FnOnce() -> Fut,
        Fut: Future<Output = Result<T, E>>,
        E: std::fmt::Display,
    {
        // Try cache first
        if let Ok(Some(cached)) = CacheRepository::get::<T>(&self.db, key, max_age_secs).await {
            return Ok(cached);
        }

        // Fetch fresh data
        let data = fetcher().await?;

        // Cache the result (log but don't fail on cache errors)
        if let Err(e) = CacheRepository::set(&self.db, key, &data).await {
            tracing::warn!("Failed to cache data for key '{}': {}", key, e);
        }

        Ok(data)
    }

    /// Get cached data without fetching
    pub async fn get<T: DeserializeOwned>(
        &self,
        key: &str,
        max_age_secs: i64,
    ) -> Result<Option<T>, CacheError> {
        Ok(CacheRepository::get(&self.db, key, max_age_secs).await?)
    }

    /// Set cached data
    pub async fn set<T: Serialize>(&self, key: &str, data: &T) -> Result<(), CacheError> {
        Ok(CacheRepository::set(&self.db, key, data).await?)
    }

    /// Clean up expired cache entries
    pub async fn cleanup(&self, max_age_secs: i64) -> Result<u64, CacheError> {
        Ok(CacheRepository::cleanup(&self.db, max_age_secs).await?)
    }
}
