use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateLog, Log, LogLevel, LogQueryParams};

/// Common SELECT fields for log queries
const SELECT_LOG: &str = r#"
    SELECT
        id, created_at, level, message
    FROM log
"#;

pub struct LogRepository;

impl LogRepository {
    /// Create a new log
    pub async fn create(pool: &SqlitePool, data: CreateLog) -> Result<Log, sqlx::Error> {
        let result = sqlx::query(
            r#"
            INSERT INTO log (level, message)
            VALUES ($1, $2)
            RETURNING id
            "#,
        )
        .bind(data.level.as_str())
        .bind(&data.message)
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Get a log by ID
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Log>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_LOG);
        let row = sqlx::query_as::<_, LogRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// List logs with optional filtering and pagination
    pub async fn list(pool: &SqlitePool, params: LogQueryParams) -> Result<Vec<Log>, sqlx::Error> {
        let limit = params.limit.unwrap_or(50).min(500);
        let offset = params.offset.unwrap_or(0);

        // Build dynamic query with parameter bindings
        let mut query = SELECT_LOG.to_string();

        let rows = if let Some(level) = &params.level {
            query.push_str(" WHERE level = $1 ORDER BY created_at DESC LIMIT $2 OFFSET $3");
            sqlx::query_as::<_, LogRow>(&query)
                .bind(level)
                .bind(limit)
                .bind(offset)
                .fetch_all(pool)
                .await?
        } else {
            query.push_str(" ORDER BY created_at DESC LIMIT $1 OFFSET $2");
            sqlx::query_as::<_, LogRow>(&query)
                .bind(limit)
                .bind(offset)
                .fetch_all(pool)
                .await?
        };

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get recent logs (for SSE initial push)
    pub async fn get_recent(pool: &SqlitePool, limit: i64) -> Result<Vec<Log>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC LIMIT $1", SELECT_LOG);
        let rows = sqlx::query_as::<_, LogRow>(&query)
            .bind(limit)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Delete logs older than given number of days
    pub async fn cleanup_old(pool: &SqlitePool, days: i64) -> Result<u64, sqlx::Error> {
        let result = sqlx::query(
            r#"
            DELETE FROM log
            WHERE created_at < datetime('now', '-' || $1 || ' days')
            "#,
        )
        .bind(days)
        .execute(pool)
        .await?;

        Ok(result.rows_affected())
    }

    /// Delete all logs
    pub async fn delete_all(pool: &SqlitePool) -> Result<u64, sqlx::Error> {
        let result = sqlx::query("DELETE FROM log").execute(pool).await?;

        Ok(result.rows_affected())
    }

    /// Count logs by level
    pub async fn count_by_level(pool: &SqlitePool, level: LogLevel) -> Result<i64, sqlx::Error> {
        let row: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM log WHERE level = $1")
            .bind(level.as_str())
            .fetch_one(pool)
            .await?;

        Ok(row.0)
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct LogRow {
    id: i64,
    created_at: DateTime<Utc>,
    level: String,
    message: String,
}

impl From<LogRow> for Log {
    fn from(row: LogRow) -> Self {
        Self {
            id: row.id,
            created_at: row.created_at,
            level: row.level.parse().unwrap_or_default(),
            message: row.message,
        }
    }
}
