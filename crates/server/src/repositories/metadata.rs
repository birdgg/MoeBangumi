use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateMetadata, Metadata, UpdateMetadata};

/// Common SELECT fields for metadata queries
const SELECT_METADATA: &str = r#"
    SELECT
        id, created_at, updated_at,
        mikan_id, bgmtv_id, tmdb_id,
        title_chinese, title_japanese,
        season, year, platform,
        total_episodes, poster_url, air_date, air_week
    FROM metadata
"#;

pub struct MetadataRepository;

impl MetadataRepository {
    /// Create new metadata
    pub async fn create(pool: &SqlitePool, data: CreateMetadata) -> Result<Metadata, sqlx::Error> {
        let result = sqlx::query(
            r#"
            INSERT INTO metadata (
                mikan_id, bgmtv_id, tmdb_id,
                title_chinese, title_japanese,
                season, year, platform,
                total_episodes, poster_url, air_date, air_week
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
            RETURNING id
            "#,
        )
        .bind(&data.mikan_id)
        .bind(data.bgmtv_id)
        .bind(data.tmdb_id)
        .bind(&data.title_chinese)
        .bind(&data.title_japanese)
        .bind(data.season)
        .bind(data.year)
        .bind(data.platform.as_str())
        .bind(data.total_episodes)
        .bind(&data.poster_url)
        .bind(&data.air_date)
        .bind(data.air_week)
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Get metadata by ID
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Metadata>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_METADATA);
        let row = sqlx::query_as::<_, MetadataRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get metadata by Mikan ID
    pub async fn get_by_mikan_id(
        pool: &SqlitePool,
        mikan_id: &str,
    ) -> Result<Option<Metadata>, sqlx::Error> {
        let query = format!("{} WHERE mikan_id = $1", SELECT_METADATA);
        let row = sqlx::query_as::<_, MetadataRow>(&query)
            .bind(mikan_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get metadata by BGM.tv ID
    pub async fn get_by_bgmtv_id(
        pool: &SqlitePool,
        bgmtv_id: i64,
    ) -> Result<Option<Metadata>, sqlx::Error> {
        let query = format!("{} WHERE bgmtv_id = $1", SELECT_METADATA);
        let row = sqlx::query_as::<_, MetadataRow>(&query)
            .bind(bgmtv_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get metadata by TMDB ID
    pub async fn get_by_tmdb_id(
        pool: &SqlitePool,
        tmdb_id: i64,
    ) -> Result<Option<Metadata>, sqlx::Error> {
        let query = format!("{} WHERE tmdb_id = $1", SELECT_METADATA);
        let row = sqlx::query_as::<_, MetadataRow>(&query)
            .bind(tmdb_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get all metadata
    pub async fn get_all(pool: &SqlitePool) -> Result<Vec<Metadata>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC", SELECT_METADATA);
        let rows = sqlx::query_as::<_, MetadataRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Update metadata
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateMetadata,
    ) -> Result<Option<Metadata>, sqlx::Error> {
        let existing = Self::get_by_id(pool, id).await?;
        let Some(existing) = existing else {
            return Ok(None);
        };

        let mikan_id = data.mikan_id.resolve(existing.mikan_id);
        let bgmtv_id = data.bgmtv_id.resolve(existing.bgmtv_id);
        let tmdb_id = data.tmdb_id.resolve(existing.tmdb_id);
        let title_chinese = data.title_chinese.unwrap_or(existing.title_chinese);
        let title_japanese = data.title_japanese.resolve(existing.title_japanese);
        let season = data.season.unwrap_or(existing.season);
        let year = data.year.unwrap_or(existing.year);
        let platform = data.platform.unwrap_or(existing.platform);
        let total_episodes = data.total_episodes.unwrap_or(existing.total_episodes);
        let poster_url = data.poster_url.resolve(existing.poster_url);
        let air_date = data.air_date.resolve(existing.air_date);
        let air_week = data.air_week.unwrap_or(existing.air_week);

        sqlx::query(
            r#"
            UPDATE metadata SET
                mikan_id = $1,
                bgmtv_id = $2,
                tmdb_id = $3,
                title_chinese = $4,
                title_japanese = $5,
                season = $6,
                year = $7,
                platform = $8,
                total_episodes = $9,
                poster_url = $10,
                air_date = $11,
                air_week = $12
            WHERE id = $13
            "#,
        )
        .bind(&mikan_id)
        .bind(bgmtv_id)
        .bind(tmdb_id)
        .bind(&title_chinese)
        .bind(&title_japanese)
        .bind(season)
        .bind(year)
        .bind(platform.as_str())
        .bind(total_episodes)
        .bind(&poster_url)
        .bind(&air_date)
        .bind(air_week)
        .bind(id)
        .execute(pool)
        .await?;

        Self::get_by_id(pool, id).await
    }

    /// Delete metadata by ID
    pub async fn delete(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("DELETE FROM metadata WHERE id = $1")
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Update poster URL for metadata
    pub async fn update_poster_url(
        pool: &SqlitePool,
        id: i64,
        poster_url: &str,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("UPDATE metadata SET poster_url = $1 WHERE id = $2")
            .bind(poster_url)
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Batch upsert for Mikan mapping sync
    /// Inserts new mappings with minimal data (mikan_id, bgmtv_id, title)
    /// Returns number of newly inserted records
    pub async fn upsert_batch_mikan(
        pool: &SqlitePool,
        rows: &[(String, i64, String, i32)], // (mikan_id, bgmtv_id, title_chinese, air_week)
    ) -> Result<usize, sqlx::Error> {
        let mut tx = pool.begin().await?;
        let mut inserted = 0;

        for (mikan_id, bgmtv_id, title_chinese, air_week) in rows {
            // Use current year as default
            let year = chrono::Utc::now().format("%Y").to_string().parse::<i32>().unwrap_or(2024);

            let result = sqlx::query(
                r#"
                INSERT OR IGNORE INTO metadata (
                    mikan_id, bgmtv_id, title_chinese, year, air_week
                )
                VALUES ($1, $2, $3, $4, $5)
                "#,
            )
            .bind(mikan_id)
            .bind(bgmtv_id)
            .bind(title_chinese)
            .bind(year)
            .bind(air_week)
            .execute(&mut *tx)
            .await?;

            if result.rows_affected() > 0 {
                inserted += 1;
            }
        }

        tx.commit().await?;
        Ok(inserted)
    }

    /// Check if Mikan ID exists
    pub async fn mikan_id_exists(pool: &SqlitePool, mikan_id: &str) -> Result<bool, sqlx::Error> {
        let count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM metadata WHERE mikan_id = $1")
            .bind(mikan_id)
            .fetch_one(pool)
            .await?;
        Ok(count.0 > 0)
    }

    /// Count total metadata records
    pub async fn count(pool: &SqlitePool) -> Result<i64, sqlx::Error> {
        let count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM metadata")
            .fetch_one(pool)
            .await?;
        Ok(count.0)
    }

    /// Get metadata records with remote poster URLs (not starting with /posters/)
    pub async fn get_remote_poster_metadata(
        pool: &SqlitePool,
    ) -> Result<Vec<(i64, String)>, sqlx::Error> {
        let rows: Vec<(i64, String)> = sqlx::query_as(
            r#"
            SELECT id, poster_url
            FROM metadata
            WHERE poster_url IS NOT NULL
              AND poster_url != ''
              AND poster_url NOT LIKE '/posters/%'
            "#,
        )
        .fetch_all(pool)
        .await?;

        Ok(rows)
    }

    /// Get metadata records without TMDB ID but with Japanese title
    pub async fn get_metadata_without_tmdb_id(
        pool: &SqlitePool,
    ) -> Result<Vec<(i64, String)>, sqlx::Error> {
        let rows: Vec<(i64, String)> = sqlx::query_as(
            r#"
            SELECT id, title_japanese
            FROM metadata
            WHERE tmdb_id IS NULL
              AND title_japanese IS NOT NULL
              AND title_japanese != ''
            "#,
        )
        .fetch_all(pool)
        .await?;

        Ok(rows)
    }

    /// Update TMDB ID for metadata
    pub async fn update_tmdb_id(
        pool: &SqlitePool,
        id: i64,
        tmdb_id: i64,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("UPDATE metadata SET tmdb_id = $1 WHERE id = $2")
            .bind(tmdb_id)
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct MetadataRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    mikan_id: Option<String>,
    bgmtv_id: Option<i64>,
    tmdb_id: Option<i64>,
    title_chinese: String,
    title_japanese: Option<String>,
    season: i32,
    year: i32,
    platform: String,
    total_episodes: i32,
    poster_url: Option<String>,
    air_date: Option<String>,
    air_week: i32,
}

impl From<MetadataRow> for Metadata {
    fn from(row: MetadataRow) -> Self {
        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            mikan_id: row.mikan_id,
            bgmtv_id: row.bgmtv_id,
            tmdb_id: row.tmdb_id,
            title_chinese: row.title_chinese,
            title_japanese: row.title_japanese,
            season: row.season,
            year: row.year,
            platform: row.platform.parse().unwrap_or_default(),
            total_episodes: row.total_episodes,
            poster_url: row.poster_url,
            air_date: row.air_date,
            air_week: row.air_week,
        }
    }
}
