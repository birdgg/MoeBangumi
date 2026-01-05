use sqlx::{sqlite::SqlitePoolOptions, SqlitePool};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DatabaseError {
    #[error("Failed to connect to database '{url}': {source} (check if data directory is writable)")]
    Connection { url: String, source: sqlx::Error },
    #[error("Failed to run database migrations: {0}")]
    Migration(#[from] sqlx::migrate::MigrateError),
}

pub async fn create_pool(database_url: &str) -> Result<SqlitePool, DatabaseError> {
    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect(database_url)
        .await
        .map_err(|e| DatabaseError::Connection {
            url: database_url.to_string(),
            source: e,
        })?;

    // Enable WAL mode for better concurrency (allows concurrent reads during writes)
    sqlx::query("PRAGMA journal_mode=WAL")
        .execute(&pool)
        .await
        .map_err(|e| DatabaseError::Connection {
            url: database_url.to_string(),
            source: e,
        })?;

    // Set busy timeout to 5 seconds (wait for lock instead of failing immediately)
    sqlx::query("PRAGMA busy_timeout=5000")
        .execute(&pool)
        .await
        .map_err(|e| DatabaseError::Connection {
            url: database_url.to_string(),
            source: e,
        })?;

    // Run migrations
    sqlx::migrate!("../../migrations").run(&pool).await?;

    Ok(pool)
}
