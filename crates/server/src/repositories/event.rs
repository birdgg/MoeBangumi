use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateEvent, Event, EventLevel, EventQueryParams};

/// Common SELECT fields for event queries
const SELECT_EVENT: &str = r#"
    SELECT
        id, created_at, level, message, details
    FROM event
"#;

pub struct EventRepository;

impl EventRepository {
    /// Create a new event
    pub async fn create(pool: &SqlitePool, data: CreateEvent) -> Result<Event, sqlx::Error> {
        let result = sqlx::query(
            r#"
            INSERT INTO event (level, message, details)
            VALUES ($1, $2, $3)
            RETURNING id
            "#,
        )
        .bind(data.level.as_str())
        .bind(&data.message)
        .bind(&data.details)
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Get an event by ID
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Event>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_EVENT);
        let row = sqlx::query_as::<_, EventRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// List events with optional filtering and pagination
    pub async fn list(
        pool: &SqlitePool,
        params: EventQueryParams,
    ) -> Result<Vec<Event>, sqlx::Error> {
        let limit = params.limit.unwrap_or(50).min(500);
        let offset = params.offset.unwrap_or(0);

        // Build dynamic query
        let mut query = SELECT_EVENT.to_string();

        if params.level.is_some() {
            query.push_str(" WHERE level = $1");
        }

        query.push_str(" ORDER BY created_at DESC");
        query.push_str(&format!(" LIMIT {} OFFSET {}", limit, offset));

        // Execute with appropriate bindings
        let rows = if let Some(level) = &params.level {
            sqlx::query_as::<_, EventRow>(&query)
                .bind(level)
                .fetch_all(pool)
                .await?
        } else {
            sqlx::query_as::<_, EventRow>(&query)
                .fetch_all(pool)
                .await?
        };

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get recent events (for SSE initial push)
    pub async fn get_recent(pool: &SqlitePool, limit: i64) -> Result<Vec<Event>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC LIMIT $1", SELECT_EVENT);
        let rows = sqlx::query_as::<_, EventRow>(&query)
            .bind(limit)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Delete events older than given number of days
    pub async fn cleanup_old(pool: &SqlitePool, days: i64) -> Result<u64, sqlx::Error> {
        let result = sqlx::query(
            r#"
            DELETE FROM event
            WHERE created_at < datetime('now', '-' || $1 || ' days')
            "#,
        )
        .bind(days)
        .execute(pool)
        .await?;

        Ok(result.rows_affected())
    }

    /// Delete all events
    pub async fn delete_all(pool: &SqlitePool) -> Result<u64, sqlx::Error> {
        let result = sqlx::query("DELETE FROM event").execute(pool).await?;

        Ok(result.rows_affected())
    }

    /// Count events by level
    pub async fn count_by_level(pool: &SqlitePool, level: EventLevel) -> Result<i64, sqlx::Error> {
        let row: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM event WHERE level = $1")
            .bind(level.as_str())
            .fetch_one(pool)
            .await?;

        Ok(row.0)
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct EventRow {
    id: i64,
    created_at: DateTime<Utc>,
    level: String,
    message: String,
    details: Option<String>,
}

impl From<EventRow> for Event {
    fn from(row: EventRow) -> Self {
        Self {
            id: row.id,
            created_at: row.created_at,
            level: row.level.parse().unwrap_or_default(),
            message: row.message,
            details: row.details,
        }
    }
}
