use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateRss, Rss, UpdateRss};

/// Common SELECT fields for RSS queries
const SELECT_RSS: &str = r#"
    SELECT
        id, created_at, updated_at,
        bangumi_id, title, url, save_path, enabled, exclude_filters
    FROM rss
"#;

pub struct RssRepository;

impl RssRepository {
    /// Create a new RSS subscription
    pub async fn create(pool: &SqlitePool, data: CreateRss) -> Result<Rss, sqlx::Error> {
        let exclude_filters_json = serde_json::to_string(&data.exclude_filters)
            .unwrap_or_else(|_| "[]".to_string());

        let result = sqlx::query(
            r#"
            INSERT INTO rss (bangumi_id, title, url, save_path, enabled, exclude_filters)
            VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING id
            "#,
        )
        .bind(data.bangumi_id)
        .bind(&data.title)
        .bind(&data.url)
        .bind(&data.save_path)
        .bind(data.enabled)
        .bind(&exclude_filters_json)
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
    pub async fn get_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<Vec<Rss>, sqlx::Error> {
        let query = format!("{} WHERE bangumi_id = $1 ORDER BY created_at DESC", SELECT_RSS);
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

        let title = data.title.unwrap_or(existing.title);
        let url = data.url.unwrap_or(existing.url);
        let save_path = data.save_path.unwrap_or(existing.save_path);
        let enabled = data.enabled.unwrap_or(existing.enabled);
        let exclude_filters = data.exclude_filters.unwrap_or(existing.exclude_filters);
        let exclude_filters_json = serde_json::to_string(&exclude_filters)
            .unwrap_or_else(|_| "[]".to_string());

        sqlx::query(
            r#"
            UPDATE rss SET
                title = $1,
                url = $2,
                save_path = $3,
                enabled = $4,
                exclude_filters = $5
            WHERE id = $6
            "#,
        )
        .bind(&title)
        .bind(&url)
        .bind(&save_path)
        .bind(enabled)
        .bind(&exclude_filters_json)
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
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct RssRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    bangumi_id: i64,
    title: String,
    url: String,
    save_path: String,
    enabled: bool,
    exclude_filters: String,
}

impl From<RssRow> for Rss {
    fn from(row: RssRow) -> Self {
        let exclude_filters: Vec<String> = serde_json::from_str(&row.exclude_filters)
            .unwrap_or_default();

        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            bangumi_id: row.bangumi_id,
            title: row.title,
            url: row.url,
            save_path: row.save_path,
            enabled: row.enabled,
            exclude_filters,
        }
    }
}
