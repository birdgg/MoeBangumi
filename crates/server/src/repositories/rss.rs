use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateRss, Rss, UpdateRss};

/// Common SELECT fields for RSS queries
const SELECT_RSS: &str = r#"
    SELECT
        id, created_at, updated_at,
        bangumi_id, url, enabled, exclude_filters, include_filters, is_primary, "group"
    FROM rss
"#;

pub struct RssRepository;

impl RssRepository {
    /// Create a new RSS subscription
    pub async fn create(pool: &SqlitePool, data: CreateRss) -> Result<Rss, sqlx::Error> {
        let exclude_filters_json = serde_json::to_string(&data.exclude_filters)
            .unwrap_or_else(|_| "[]".to_string());
        let include_filters_json = serde_json::to_string(&data.include_filters)
            .unwrap_or_else(|_| "[]".to_string());

        // If creating a primary RSS, demote any existing primary for this bangumi
        if data.is_primary {
            Self::demote_primary(pool, data.bangumi_id).await?;
        }

        let result = sqlx::query(
            r#"
            INSERT INTO rss (bangumi_id, url, enabled, exclude_filters, include_filters, is_primary, "group")
            VALUES ($1, $2, $3, $4, $5, $6, $7)
            RETURNING id
            "#,
        )
        .bind(data.bangumi_id)
        .bind(&data.url)
        .bind(data.enabled)
        .bind(&exclude_filters_json)
        .bind(&include_filters_json)
        .bind(data.is_primary)
        .bind(&data.group)
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Get an RSS subscription by ID
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Rss>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_RSS);
        let row = sqlx::query_as::<_, RssRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get all RSS subscriptions for a bangumi
    /// Primary RSS is returned first, followed by backups ordered by creation time
    pub async fn get_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<Vec<Rss>, sqlx::Error> {
        let query = format!(
            "{} WHERE bangumi_id = $1 ORDER BY is_primary DESC, created_at DESC",
            SELECT_RSS
        );
        let rows = sqlx::query_as::<_, RssRow>(&query)
            .bind(bangumi_id)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get all RSS subscriptions
    pub async fn get_all(pool: &SqlitePool) -> Result<Vec<Rss>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC", SELECT_RSS);
        let rows = sqlx::query_as::<_, RssRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get all enabled RSS subscriptions
    pub async fn get_enabled(pool: &SqlitePool) -> Result<Vec<Rss>, sqlx::Error> {
        let query = format!("{} WHERE enabled = 1 ORDER BY created_at DESC", SELECT_RSS);
        let rows = sqlx::query_as::<_, RssRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Update an RSS subscription
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateRss,
    ) -> Result<Option<Rss>, sqlx::Error> {
        let existing = Self::get_by_id(pool, id).await?;
        let Some(existing) = existing else {
            return Ok(None);
        };

        let url = data.url.unwrap_or(existing.url);
        let enabled = data.enabled.unwrap_or(existing.enabled);
        let exclude_filters = data.exclude_filters.unwrap_or(existing.exclude_filters);
        let exclude_filters_json = serde_json::to_string(&exclude_filters)
            .unwrap_or_else(|_| "[]".to_string());
        let include_filters = data.include_filters.unwrap_or(existing.include_filters);
        let include_filters_json = serde_json::to_string(&include_filters)
            .unwrap_or_else(|_| "[]".to_string());
        let is_primary = data.is_primary.unwrap_or(existing.is_primary);
        let group = data.group.or(existing.group);

        // If promoting to primary, demote any existing primary for this bangumi
        if is_primary && !existing.is_primary {
            Self::demote_primary(pool, existing.bangumi_id).await?;
        }

        sqlx::query(
            r#"
            UPDATE rss SET
                url = $1,
                enabled = $2,
                exclude_filters = $3,
                include_filters = $4,
                is_primary = $5,
                "group" = $6
            WHERE id = $7
            "#,
        )
        .bind(&url)
        .bind(enabled)
        .bind(&exclude_filters_json)
        .bind(&include_filters_json)
        .bind(is_primary)
        .bind(&group)
        .bind(id)
        .execute(pool)
        .await?;

        Self::get_by_id(pool, id).await
    }

    /// Delete an RSS subscription by ID
    pub async fn delete(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("DELETE FROM rss WHERE id = $1")
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Delete all RSS subscriptions for a bangumi
    pub async fn delete_by_bangumi_id(pool: &SqlitePool, bangumi_id: i64) -> Result<u64, sqlx::Error> {
        let result = sqlx::query("DELETE FROM rss WHERE bangumi_id = $1")
            .bind(bangumi_id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected())
    }

    /// Demote any existing primary RSS for a bangumi to backup
    /// This is called before creating/updating a new primary RSS
    async fn demote_primary(pool: &SqlitePool, bangumi_id: i64) -> Result<(), sqlx::Error> {
        sqlx::query("UPDATE rss SET is_primary = 0 WHERE bangumi_id = $1 AND is_primary = 1")
            .bind(bangumi_id)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// Get the primary RSS subscription for a bangumi
    /// Returns None if no primary RSS is set
    pub async fn get_primary_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<Option<Rss>, sqlx::Error> {
        let query = format!("{} WHERE bangumi_id = $1 AND is_primary = 1", SELECT_RSS);
        let row = sqlx::query_as::<_, RssRow>(&query)
            .bind(bangumi_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct RssRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    bangumi_id: i64,
    url: String,
    enabled: bool,
    exclude_filters: String,
    include_filters: String,
    is_primary: bool,
    group: Option<String>,
}

impl From<RssRow> for Rss {
    fn from(row: RssRow) -> Self {
        let exclude_filters: Vec<String> = serde_json::from_str(&row.exclude_filters)
            .unwrap_or_default();
        let include_filters: Vec<String> = serde_json::from_str(&row.include_filters)
            .unwrap_or_default();

        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            bangumi_id: row.bangumi_id,
            url: row.url,
            enabled: row.enabled,
            exclude_filters,
            include_filters,
            is_primary: row.is_primary,
            group: row.group,
        }
    }
}
