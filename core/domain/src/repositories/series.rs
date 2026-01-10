use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateSeries, Series, UpdateSeries};

/// Common SELECT fields for series queries
const SELECT_SERIES: &str = r#"
    SELECT
        id, created_at, updated_at,
        tmdb_id, title_chinese, title_japanese, poster_url
    FROM series
"#;

pub struct SeriesRepository;

impl SeriesRepository {
    /// Create a new series
    pub async fn create(pool: &SqlitePool, data: CreateSeries) -> Result<Series, sqlx::Error> {
        let result = sqlx::query(
            r#"
            INSERT INTO series (tmdb_id, title_chinese, title_japanese, poster_url)
            VALUES ($1, $2, $3, $4)
            RETURNING id
            "#,
        )
        .bind(data.tmdb_id)
        .bind(&data.title_chinese)
        .bind(&data.title_japanese)
        .bind(&data.poster_url)
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Get a series by ID
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Series>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_SERIES);
        let row = sqlx::query_as::<_, SeriesRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get a series by TMDB ID
    pub async fn get_by_tmdb_id(
        pool: &SqlitePool,
        tmdb_id: i64,
    ) -> Result<Option<Series>, sqlx::Error> {
        let query = format!("{} WHERE tmdb_id = $1", SELECT_SERIES);
        let row = sqlx::query_as::<_, SeriesRow>(&query)
            .bind(tmdb_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get a series by title (exact match)
    pub async fn get_by_title(
        pool: &SqlitePool,
        title_chinese: &str,
    ) -> Result<Option<Series>, sqlx::Error> {
        let query = format!("{} WHERE title_chinese = $1", SELECT_SERIES);
        let row = sqlx::query_as::<_, SeriesRow>(&query)
            .bind(title_chinese)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get all series
    pub async fn get_all(pool: &SqlitePool) -> Result<Vec<Series>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC", SELECT_SERIES);
        let rows = sqlx::query_as::<_, SeriesRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Update a series
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateSeries,
    ) -> Result<Option<Series>, sqlx::Error> {
        let tmdb_id_update = data.tmdb_id.should_update();
        let title_japanese_update = data.title_japanese.should_update();
        let poster_url_update = data.poster_url.should_update();

        let result = sqlx::query(
            r#"
            UPDATE series SET
                tmdb_id = CASE WHEN $1 THEN $2 ELSE tmdb_id END,
                title_chinese = COALESCE($3, title_chinese),
                title_japanese = CASE WHEN $4 THEN $5 ELSE title_japanese END,
                poster_url = CASE WHEN $6 THEN $7 ELSE poster_url END
            WHERE id = $8
            "#,
        )
        .bind(tmdb_id_update)
        .bind(data.tmdb_id.into_value())
        .bind(&data.title_chinese)
        .bind(title_japanese_update)
        .bind(data.title_japanese.into_value())
        .bind(poster_url_update)
        .bind(data.poster_url.into_value())
        .bind(id)
        .execute(pool)
        .await?;

        if result.rows_affected() == 0 {
            return Ok(None);
        }

        Self::get_by_id(pool, id).await
    }

    /// Delete a series by ID
    pub async fn delete(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("DELETE FROM series WHERE id = $1")
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Find or create a series by title
    /// Returns (series, created) where created is true if a new series was created
    pub async fn find_or_create(
        pool: &SqlitePool,
        data: CreateSeries,
    ) -> Result<(Series, bool), sqlx::Error> {
        // First try to find by TMDB ID
        if let Some(tmdb_id) = data.tmdb_id {
            if let Some(series) = Self::get_by_tmdb_id(pool, tmdb_id).await? {
                return Ok((series, false));
            }
        }

        // Then try to find by title
        if let Some(series) = Self::get_by_title(pool, &data.title_chinese).await? {
            return Ok((series, false));
        }

        // Create new series
        let series = Self::create(pool, data).await?;
        Ok((series, true))
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct SeriesRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    tmdb_id: Option<i64>,
    title_chinese: String,
    title_japanese: Option<String>,
    poster_url: Option<String>,
}

impl From<SeriesRow> for Series {
    fn from(row: SeriesRow) -> Self {
        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            tmdb_id: row.tmdb_id,
            title_chinese: row.title_chinese,
            title_japanese: row.title_japanese,
            poster_url: row.poster_url,
        }
    }
}
