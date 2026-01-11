use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{Bangumi, Platform, UpdateBangumi};

/// Common SELECT fields for bangumi queries (merged with metadata fields)
const SELECT_BANGUMI: &str = r#"
    SELECT
        id, created_at, updated_at,
        mikan_id, bgmtv_id, tmdb_id,
        title_chinese, title_japanese,
        season, year, platform,
        total_episodes, poster_url, air_date, air_week,
        tmdb_lookup_at, episode_offset,
        current_episode, auto_complete, save_path, source_type
    FROM bangumi
"#;

/// Data required to create a new bangumi
pub struct CreateBangumiData {
    // External IDs
    pub mikan_id: Option<String>,
    pub bgmtv_id: Option<i64>,
    pub tmdb_id: Option<i64>,
    // Titles
    pub title_chinese: String,
    pub title_japanese: Option<String>,
    // Basic info
    pub season: i32,
    pub year: i32,
    pub platform: String,
    // Metadata
    pub total_episodes: i32,
    pub poster_url: Option<String>,
    pub air_date: Option<String>,
    pub air_week: i32,
    pub episode_offset: i32,
    // Subscription
    pub current_episode: Option<i32>,
    pub auto_complete: bool,
    pub save_path: String,
    pub source_type: String,
}

pub struct BangumiRepository;

impl BangumiRepository {
    /// Create a new bangumi
    pub async fn create(
        pool: &SqlitePool,
        data: CreateBangumiData,
    ) -> Result<Bangumi, sqlx::Error> {
        let result = sqlx::query(
            r#"
            INSERT INTO bangumi (
                mikan_id, bgmtv_id, tmdb_id,
                title_chinese, title_japanese,
                season, year, platform,
                total_episodes, poster_url, air_date, air_week, episode_offset,
                current_episode, auto_complete, save_path, source_type
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, COALESCE($14, 0), $15, $16, $17)
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
        .bind(&data.platform)
        .bind(data.total_episodes)
        .bind(&data.poster_url)
        .bind(&data.air_date)
        .bind(data.air_week)
        .bind(data.episode_offset)
        .bind(data.current_episode)
        .bind(data.auto_complete)
        .bind(&data.save_path)
        .bind(&data.source_type)
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Batch create bangumi records
    /// Returns a map of bgmtv_id -> bangumi_id for created records
    pub async fn batch_create(
        pool: &SqlitePool,
        entries: Vec<CreateBangumiData>,
    ) -> Result<std::collections::HashMap<i64, i64>, sqlx::Error> {
        if entries.is_empty() {
            return Ok(std::collections::HashMap::new());
        }

        let mut tx = pool.begin().await?;
        let mut result_map = std::collections::HashMap::new();

        for data in entries {
            let bgmtv_id = match data.bgmtv_id {
                Some(id) => id,
                None => continue,
            };

            let result = sqlx::query(
                r#"
                INSERT INTO bangumi (
                    mikan_id, bgmtv_id, tmdb_id,
                    title_chinese, title_japanese,
                    season, year, platform,
                    total_episodes, poster_url, air_date, air_week, episode_offset,
                    current_episode, auto_complete, save_path, source_type
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, COALESCE($14, 0), $15, $16, $17)
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
            .bind(&data.platform)
            .bind(data.total_episodes)
            .bind(&data.poster_url)
            .bind(&data.air_date)
            .bind(data.air_week)
            .bind(data.episode_offset)
            .bind(data.current_episode)
            .bind(data.auto_complete)
            .bind(&data.save_path)
            .bind(&data.source_type)
            .fetch_one(&mut *tx)
            .await?;

            let id: i64 = sqlx::Row::get(&result, "id");
            result_map.insert(bgmtv_id, id);
        }

        tx.commit().await?;
        Ok(result_map)
    }

    /// Get a bangumi by ID
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Bangumi>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_BANGUMI);
        let row = sqlx::query_as::<_, BangumiRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get bangumi by Mikan ID
    pub async fn get_by_mikan_id(
        pool: &SqlitePool,
        mikan_id: &str,
    ) -> Result<Option<Bangumi>, sqlx::Error> {
        let query = format!("{} WHERE mikan_id = $1", SELECT_BANGUMI);
        let row = sqlx::query_as::<_, BangumiRow>(&query)
            .bind(mikan_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get bangumi by BGM.tv ID
    pub async fn get_by_bgmtv_id(
        pool: &SqlitePool,
        bgmtv_id: i64,
    ) -> Result<Option<Bangumi>, sqlx::Error> {
        let query = format!("{} WHERE bgmtv_id = $1", SELECT_BANGUMI);
        let row = sqlx::query_as::<_, BangumiRow>(&query)
            .bind(bgmtv_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Batch get bangumi IDs by BGM.tv IDs
    /// Returns a map of bgmtv_id -> bangumi_id for existing records
    pub async fn get_ids_by_bgmtv_ids(
        pool: &SqlitePool,
        bgmtv_ids: &[i64],
    ) -> Result<std::collections::HashMap<i64, i64>, sqlx::Error> {
        if bgmtv_ids.is_empty() {
            return Ok(std::collections::HashMap::new());
        }

        let placeholders: String = bgmtv_ids
            .iter()
            .enumerate()
            .map(|(i, _)| format!("${}", i + 1))
            .collect::<Vec<_>>()
            .join(",");

        let query = format!(
            "SELECT id, bgmtv_id FROM bangumi WHERE bgmtv_id IN ({})",
            placeholders
        );

        let mut q = sqlx::query_as::<_, (i64, i64)>(&query);
        for id in bgmtv_ids {
            q = q.bind(id);
        }

        let rows: Vec<(i64, i64)> = q.fetch_all(pool).await?;
        Ok(rows
            .into_iter()
            .map(|(id, bgmtv_id)| (bgmtv_id, id))
            .collect())
    }

    /// Get bangumi by TMDB ID
    pub async fn get_by_tmdb_id(
        pool: &SqlitePool,
        tmdb_id: i64,
    ) -> Result<Option<Bangumi>, sqlx::Error> {
        let query = format!("{} WHERE tmdb_id = $1", SELECT_BANGUMI);
        let row = sqlx::query_as::<_, BangumiRow>(&query)
            .bind(tmdb_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get bangumi by external ID (priority: mikan_id > bgmtv_id > tmdb_id)
    pub async fn get_by_external_id(
        pool: &SqlitePool,
        mikan_id: Option<&str>,
        bgmtv_id: Option<i64>,
        tmdb_id: Option<i64>,
    ) -> Result<Option<Bangumi>, sqlx::Error> {
        // Try mikan_id first
        if let Some(mikan_id) = mikan_id {
            if let Some(bangumi) = Self::get_by_mikan_id(pool, mikan_id).await? {
                return Ok(Some(bangumi));
            }
        }

        // Try bgmtv_id
        if let Some(bgmtv_id) = bgmtv_id {
            if let Some(bangumi) = Self::get_by_bgmtv_id(pool, bgmtv_id).await? {
                return Ok(Some(bangumi));
            }
        }

        // Try tmdb_id
        if let Some(tmdb_id) = tmdb_id {
            if let Some(bangumi) = Self::get_by_tmdb_id(pool, tmdb_id).await? {
                return Ok(Some(bangumi));
            }
        }

        Ok(None)
    }

    /// Get all bangumi
    pub async fn get_all(pool: &SqlitePool) -> Result<Vec<Bangumi>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC", SELECT_BANGUMI);
        let rows = sqlx::query_as::<_, BangumiRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Update a bangumi
    /// Uses CASE/COALESCE for atomic update to avoid read-modify-write race conditions
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateBangumi,
    ) -> Result<Option<Bangumi>, sqlx::Error> {
        // Extract should_update flags for Clearable fields
        let mikan_id_update = data.mikan_id.should_update();
        let bgmtv_id_update = data.bgmtv_id.should_update();
        let tmdb_id_update = data.tmdb_id.should_update();
        let title_japanese_update = data.title_japanese.should_update();
        let poster_url_update = data.poster_url.should_update();
        let air_date_update = data.air_date.should_update();

        let result = sqlx::query(
            r#"
            UPDATE bangumi SET
                current_episode = COALESCE($1, current_episode),
                auto_complete = COALESCE($2, auto_complete),
                source_type = COALESCE($3, source_type),
                mikan_id = CASE WHEN $4 THEN $5 ELSE mikan_id END,
                bgmtv_id = CASE WHEN $6 THEN $7 ELSE bgmtv_id END,
                tmdb_id = CASE WHEN $8 THEN $9 ELSE tmdb_id END,
                title_chinese = COALESCE($10, title_chinese),
                title_japanese = CASE WHEN $11 THEN $12 ELSE title_japanese END,
                season = COALESCE($13, season),
                year = COALESCE($14, year),
                platform = COALESCE($15, platform),
                total_episodes = COALESCE($16, total_episodes),
                poster_url = CASE WHEN $17 THEN $18 ELSE poster_url END,
                air_date = CASE WHEN $19 THEN $20 ELSE air_date END,
                air_week = COALESCE($21, air_week),
                episode_offset = COALESCE($22, episode_offset)
            WHERE id = $23
            "#,
        )
        .bind(data.current_episode)
        .bind(data.auto_complete)
        .bind(data.source_type.map(|s| s.as_str().to_string()))
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

    /// Delete a bangumi by ID
    pub async fn delete(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("DELETE FROM bangumi WHERE id = $1")
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Update current episode for a bangumi
    pub async fn update_current_episode(
        pool: &SqlitePool,
        id: i64,
        episode: i32,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("UPDATE bangumi SET current_episode = $1 WHERE id = $2")
            .bind(episode)
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Update current episode for a bangumi only if the new value is greater
    pub async fn update_current_episode_if_greater(
        pool: &SqlitePool,
        id: i64,
        episode: i32,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query(
            "UPDATE bangumi SET current_episode = $1 WHERE id = $2 AND current_episode < $1",
        )
        .bind(episode)
        .bind(id)
        .execute(pool)
        .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Update poster URL for bangumi
    pub async fn update_poster_url(
        pool: &SqlitePool,
        id: i64,
        poster_url: &str,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("UPDATE bangumi SET poster_url = $1 WHERE id = $2")
            .bind(poster_url)
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Batch update poster URLs for multiple bangumi records
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
            let result = sqlx::query("UPDATE bangumi SET poster_url = $1 WHERE id = $2")
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
            let year = chrono::Utc::now()
                .format("%Y")
                .to_string()
                .parse::<i32>()
                .unwrap_or(2024);

            let result = sqlx::query(
                r#"
                INSERT OR IGNORE INTO bangumi (
                    mikan_id, bgmtv_id, title_chinese, year, air_week, save_path
                )
                VALUES ($1, $2, $3, $4, $5, '')
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
        let count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM bangumi WHERE mikan_id = $1")
            .bind(mikan_id)
            .fetch_one(pool)
            .await?;
        Ok(count.0 > 0)
    }

    /// Count total bangumi records
    pub async fn count(pool: &SqlitePool) -> Result<i64, sqlx::Error> {
        let count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM bangumi")
            .fetch_one(pool)
            .await?;
        Ok(count.0)
    }

    /// Get bangumi records that need syncing (remote poster URL)
    ///
    /// Uses keyset pagination (seek method) for efficient chunked processing.
    ///
    /// # Arguments
    /// * `pool` - Database connection pool
    /// * `limit` - Maximum number of records to return
    /// * `after_id` - Fetch records with id > after_id (use 0 to start from beginning)
    pub async fn get_bangumi_to_sync(
        pool: &SqlitePool,
        limit: i64,
        after_id: i64,
    ) -> Result<Vec<BangumiToSync>, sqlx::Error> {
        let rows: Vec<BangumiToSync> = sqlx::query_as(
            r#"
            SELECT id, title_chinese, poster_url
            FROM bangumi
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

    /// Count bangumi records that need syncing
    pub async fn count_bangumi_to_sync(pool: &SqlitePool) -> Result<i64, sqlx::Error> {
        let count: (i64,) = sqlx::query_as(
            r#"
            SELECT COUNT(*)
            FROM bangumi
            WHERE poster_url IS NOT NULL
              AND poster_url != ''
              AND poster_url NOT LIKE '/posters/%'
            "#,
        )
        .fetch_one(pool)
        .await?;

        Ok(count.0)
    }

    /// Update TMDB ID for bangumi
    pub async fn update_tmdb_id(
        pool: &SqlitePool,
        id: i64,
        tmdb_id: i64,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("UPDATE bangumi SET tmdb_id = $1 WHERE id = $2")
            .bind(tmdb_id)
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }
}

/// Bangumi record that needs syncing
#[derive(Debug, sqlx::FromRow)]
pub struct BangumiToSync {
    pub id: i64,
    pub title_chinese: String,
    pub poster_url: Option<String>,
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
pub(crate) struct BangumiRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    // External IDs
    mikan_id: Option<String>,
    bgmtv_id: Option<i64>,
    tmdb_id: Option<i64>,
    // Titles
    title_chinese: String,
    title_japanese: Option<String>,
    // Basic info
    season: i32,
    year: i32,
    platform: String,
    // Metadata
    total_episodes: i32,
    poster_url: Option<String>,
    air_date: Option<String>,
    air_week: i32,
    tmdb_lookup_at: Option<DateTime<Utc>>,
    episode_offset: i32,
    // Subscription
    current_episode: i32,
    auto_complete: bool,
    save_path: String,
    source_type: String,
}

impl From<BangumiRow> for Bangumi {
    fn from(row: BangumiRow) -> Self {
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
            platform: row.platform.parse().unwrap_or(Platform::Tv),
            total_episodes: row.total_episodes,
            poster_url: row.poster_url,
            air_date: row.air_date,
            air_week: row.air_week,
            tmdb_lookup_at: row.tmdb_lookup_at,
            episode_offset: row.episode_offset,
            current_episode: row.current_episode,
            auto_complete: row.auto_complete,
            save_path: row.save_path,
            source_type: row.source_type.parse().unwrap_or_default(),
        }
    }
}
