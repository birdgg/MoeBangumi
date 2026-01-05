use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateRss, Rss, UpdateRss};

/// Common SELECT fields for RSS queries
const SELECT_RSS: &str = r#"
    SELECT
        id, created_at, updated_at,
        bangumi_id, title, url, enabled, exclude_filters, include_filters, "subtitle_group",
        etag, last_modified, last_pub_date
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

        let result = sqlx::query(
            r#"
            INSERT INTO rss (bangumi_id, title, url, enabled, exclude_filters, include_filters, "subtitle_group")
            VALUES ($1, $2, $3, $4, $5, $6, $7)
            RETURNING id
            "#,
        )
        .bind(data.bangumi_id)
        .bind(&data.title)
        .bind(&data.url)
        .bind(data.enabled)
        .bind(&exclude_filters_json)
        .bind(&include_filters_json)
        .bind(&data.subtitle_group)
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

    /// Get multiple RSS subscriptions by IDs (batch query)
    pub async fn get_by_ids(pool: &SqlitePool, ids: &[i64]) -> Result<Vec<Rss>, sqlx::Error> {
        if ids.is_empty() {
            return Ok(Vec::new());
        }

        // SQLite has a limit on bind parameters (default 999), so we chunk the input
        const CHUNK_SIZE: usize = 500;
        let mut result = Vec::new();

        for chunk in ids.chunks(CHUNK_SIZE) {
            let placeholders: Vec<String> =
                (1..=chunk.len()).map(|i| format!("${}", i)).collect();
            let query = format!(
                "{} WHERE id IN ({})",
                SELECT_RSS,
                placeholders.join(", ")
            );

            let mut query_builder = sqlx::query_as::<_, RssRow>(&query);
            for id in chunk {
                query_builder = query_builder.bind(id);
            }

            let rows = query_builder.fetch_all(pool).await?;
            result.extend(rows.into_iter().map(Into::into));
        }

        Ok(result)
    }

    /// Get all RSS subscriptions for a bangumi
    /// Ordered by creation time (newest first)
    pub async fn get_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<Vec<Rss>, sqlx::Error> {
        let query = format!(
            "{} WHERE bangumi_id = $1 ORDER BY created_at DESC",
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
    /// Uses COALESCE for atomic update to avoid read-modify-write race conditions
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateRss,
    ) -> Result<Option<Rss>, sqlx::Error> {
        // Serialize filters to JSON if provided
        let exclude_filters_json = data
            .exclude_filters
            .as_ref()
            .map(|f| serde_json::to_string(f).unwrap_or_else(|_| "[]".to_string()));
        let include_filters_json = data
            .include_filters
            .as_ref()
            .map(|f| serde_json::to_string(f).unwrap_or_else(|_| "[]".to_string()));

        let result = sqlx::query(
            r#"
            UPDATE rss SET
                url = COALESCE($1, url),
                enabled = COALESCE($2, enabled),
                exclude_filters = COALESCE($3, exclude_filters),
                include_filters = COALESCE($4, include_filters),
                "subtitle_group" = COALESCE($5, "subtitle_group")
            WHERE id = $6
            "#,
        )
        .bind(&data.url)
        .bind(data.enabled)
        .bind(&exclude_filters_json)
        .bind(&include_filters_json)
        .bind(&data.subtitle_group)
        .bind(id)
        .execute(pool)
        .await?;

        if result.rows_affected() == 0 {
            return Ok(None);
        }

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

    /// Disable all RSS subscriptions for a bangumi
    /// Returns the number of RSS subscriptions that were disabled
    pub async fn disable_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<u64, sqlx::Error> {
        let result = sqlx::query("UPDATE rss SET enabled = 0 WHERE bangumi_id = $1 AND enabled = 1")
            .bind(bangumi_id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected())
    }

    /// Update HTTP cache information for an RSS subscription
    /// Used for incremental updates (ETag/Last-Modified/last_pub_date)
    pub async fn update_cache(
        pool: &SqlitePool,
        id: i64,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE rss SET
                etag = $1,
                last_modified = $2,
                last_pub_date = $3
            WHERE id = $4
            "#,
        )
        .bind(etag)
        .bind(last_modified)
        .bind(last_pub_date)
        .bind(id)
        .execute(pool)
        .await?;

        Ok(())
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
    enabled: bool,
    exclude_filters: String,
    include_filters: String,
    subtitle_group: Option<String>,
    etag: Option<String>,
    last_modified: Option<String>,
    last_pub_date: Option<String>,
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
            title: row.title,
            url: row.url,
            enabled: row.enabled,
            exclude_filters,
            include_filters,
            subtitle_group: row.subtitle_group,
            etag: row.etag,
            last_modified: row.last_modified,
            last_pub_date: row.last_pub_date,
        }
    }
}
