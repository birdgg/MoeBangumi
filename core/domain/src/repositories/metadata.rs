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
        total_episodes, poster_url, air_date, air_week,
        tmdb_lookup_at, episode_offset
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
                total_episodes, poster_url, air_date, air_week, episode_offset
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
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
        .bind(data.episode_offset)
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
    /// Uses CASE/COALESCE for atomic update to avoid read-modify-write race conditions
    ///
    /// For Clearable fields: CASE WHEN $should_update THEN $value ELSE field END
    /// For Option fields: COALESCE($value, field)
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateMetadata,
    ) -> Result<Option<Metadata>, sqlx::Error> {
        // Extract should_update flags for Clearable fields
        let mikan_id_update = data.mikan_id.should_update();
        let bgmtv_id_update = data.bgmtv_id.should_update();
        let tmdb_id_update = data.tmdb_id.should_update();
        let title_japanese_update = data.title_japanese.should_update();
        let poster_url_update = data.poster_url.should_update();
        let air_date_update = data.air_date.should_update();

        let result = sqlx::query(
            r#"
            UPDATE metadata SET
                mikan_id = CASE WHEN $1 THEN $2 ELSE mikan_id END,
                bgmtv_id = CASE WHEN $3 THEN $4 ELSE bgmtv_id END,
                tmdb_id = CASE WHEN $5 THEN $6 ELSE tmdb_id END,
                title_chinese = COALESCE($7, title_chinese),
                title_japanese = CASE WHEN $8 THEN $9 ELSE title_japanese END,
                season = COALESCE($10, season),
                year = COALESCE($11, year),
                platform = COALESCE($12, platform),
                total_episodes = COALESCE($13, total_episodes),
                poster_url = CASE WHEN $14 THEN $15 ELSE poster_url END,
                air_date = CASE WHEN $16 THEN $17 ELSE air_date END,
                air_week = COALESCE($18, air_week),
                episode_offset = COALESCE($19, episode_offset)
            WHERE id = $20
            "#,
        )
        .bind(mikan_id_update)
        .bind(data.mikan_id.into_value())
        .bind(bgmtv_id_update)
        .bind(data.bgmtv_id.into_value())
        .bind(tmdb_id_update)
        .bind(data.tmdb_id.into_value())
        .bind(&data.title_chinese)
        .bind(title_japanese_update)
        .bind(data.title_japanese.into_value())
        .bind(data.season)
        .bind(data.year)
        .bind(data.platform.map(|p| p.as_str().to_string()))
        .bind(data.total_episodes)
        .bind(poster_url_update)
        .bind(data.poster_url.into_value())
        .bind(air_date_update)
        .bind(data.air_date.into_value())
        .bind(data.air_week)
        .bind(data.episode_offset)
        .bind(id)
        .execute(pool)
        .await?;

        if result.rows_affected() == 0 {
            return Ok(None);
        }

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

    /// Batch update poster URLs for multiple metadata records
    ///
    /// Uses a single transaction to reduce SQLite lock contention.
    pub async fn batch_update_poster_urls(
        pool: &SqlitePool,
        updates: &[(i64, String)],
    ) -> Result<u64, sqlx::Error> {
        if updates.is_empty() {
            return Ok(0);
        }

        let mut tx = pool.begin().await?;
        let mut total_affected = 0u64;

        for (id, poster_url) in updates {
            let result = sqlx::query("UPDATE metadata SET poster_url = $1 WHERE id = $2")
                .bind(poster_url)
                .bind(id)
                .execute(&mut *tx)
                .await?;
            total_affected += result.rows_affected();
        }

        tx.commit().await?;
        Ok(total_affected)
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

    /// Get metadata records that need syncing (remote poster URL)
    ///
    /// Uses keyset pagination (seek method) for efficient chunked processing.
    ///
    /// # Arguments
    /// * `pool` - Database connection pool
    /// * `limit` - Maximum number of records to return
    /// * `after_id` - Fetch records with id > after_id (use 0 to start from beginning)
    pub async fn get_metadata_to_sync(
        pool: &SqlitePool,
        limit: i64,
        after_id: i64,
    ) -> Result<Vec<MetadataToSync>, sqlx::Error> {
        let rows: Vec<MetadataToSync> = sqlx::query_as(
            r#"
            SELECT id, title_chinese, poster_url
            FROM metadata
            WHERE id > $2
              AND poster_url IS NOT NULL
              AND poster_url != ''
              AND poster_url NOT LIKE '/posters/%'
            ORDER BY id ASC
            LIMIT $1
            "#,
        )
        .bind(limit)
        .bind(after_id)
        .fetch_all(pool)
        .await?;

        Ok(rows)
    }

    /// Count metadata records that need syncing
    pub async fn count_metadata_to_sync(pool: &SqlitePool) -> Result<i64, sqlx::Error> {
        let count: (i64,) = sqlx::query_as(
            r#"
            SELECT COUNT(*)
            FROM metadata
            WHERE poster_url IS NOT NULL
              AND poster_url != ''
              AND poster_url NOT LIKE '/posters/%'
            "#,
        )
        .fetch_one(pool)
        .await?;

        Ok(count.0)
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

/// Metadata record that needs syncing
#[derive(Debug, sqlx::FromRow)]
pub struct MetadataToSync {
    pub id: i64,
    pub title_chinese: String,
    pub poster_url: Option<String>,
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
    tmdb_lookup_at: Option<DateTime<Utc>>,
    episode_offset: i32,
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
            tmdb_lookup_at: row.tmdb_lookup_at,
            episode_offset: row.episode_offset,
        }
    }
}
