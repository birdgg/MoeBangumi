use serde::{de::DeserializeOwned, Serialize};
use sqlx::SqlitePool;
use std::time::{SystemTime, UNIX_EPOCH};

pub struct CacheRepository;

impl CacheRepository {
    /// Get cached data if not expired
    pub async fn get<T: DeserializeOwned>(
        db: &SqlitePool,
        key: &str,
        max_age_secs: i64,
    ) -> Result<Option<T>, sqlx::Error> {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        let min_time = now - max_age_secs;

        let result: Option<(String,)> = sqlx::query_as(
            "SELECT data FROM cache WHERE cache_key = ? AND fetched_at > ?",
        )
        .bind(key)
        .bind(min_time)
        .fetch_optional(db)
        .await?;

        match result {
            Some((data,)) => {
                let parsed: T = serde_json::from_str(&data).map_err(|e| {
                    tracing::error!(
                        cache_key = key,
                        error = %e,
                        "Failed to deserialize cached data"
                    );
                    sqlx::Error::Decode(Box::new(e))
                })?;
                Ok(Some(parsed))
            }
            None => Ok(None),
        }
    }

    /// Set cache data
    pub async fn set<T: Serialize>(
        db: &SqlitePool,
        key: &str,
        data: &T,
    ) -> Result<(), sqlx::Error> {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        let json = serde_json::to_string(data).map_err(|e| {
            tracing::error!(
                cache_key = key,
                error = %e,
                "Failed to serialize data for cache"
            );
            sqlx::Error::Protocol(format!("Cache serialization error: {}", e))
        })?;

        sqlx::query(
            "INSERT OR REPLACE INTO cache (cache_key, data, fetched_at) VALUES (?, ?, ?)",
        )
        .bind(key)
        .bind(json)
        .bind(now)
        .execute(db)
        .await?;

        Ok(())
    }

    /// Delete expired cache entries
    pub async fn cleanup(db: &SqlitePool, max_age_secs: i64) -> Result<u64, sqlx::Error> {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        let min_time = now - max_age_secs;

        let result = sqlx::query("DELETE FROM cache WHERE fetched_at < ?")
            .bind(min_time)
            .execute(db)
            .await?;

        Ok(result.rows_affected())
    }
}
