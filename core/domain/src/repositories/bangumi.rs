use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{Bangumi, BangumiWithSeries, Series, UpdateBangumi};

/// Common SELECT fields for bangumi queries
const SELECT_BANGUMI: &str = r#"
    SELECT
        id, created_at, updated_at,
        series_id, mikan_id, bgmtv_id,
        title_chinese, title_japanese, season, year,
        total_episodes, poster_url, air_date, air_week, platform,
        current_episode, episode_offset, auto_complete, source_type
    FROM bangumi
"#;

/// SELECT for bangumi with series JOIN
const SELECT_BANGUMI_WITH_SERIES: &str = r#"
    SELECT
        b.id, b.created_at, b.updated_at,
        b.series_id, b.mikan_id, b.bgmtv_id,
        b.title_chinese, b.title_japanese, b.season, b.year,
        b.total_episodes, b.poster_url, b.air_date, b.air_week, b.platform,
        b.current_episode, b.episode_offset, b.auto_complete, b.source_type,
        s.id as s_id, s.created_at as s_created_at, s.updated_at as s_updated_at,
        s.tmdb_id as s_tmdb_id, s.title_chinese as s_title_chinese,
        s.title_japanese as s_title_japanese, s.poster_url as s_poster_url
    FROM bangumi b
    INNER JOIN series s ON b.series_id = s.id
"#;

/// Data required to create a new bangumi
pub struct CreateBangumiData {
    pub series_id: i64,
    pub mikan_id: Option<String>,
    pub bgmtv_id: Option<i64>,
    pub title_chinese: String,
    pub title_japanese: Option<String>,
    pub season: i32,
    pub year: i32,
    pub total_episodes: i32,
    pub poster_url: Option<String>,
    pub air_date: Option<String>,
    pub air_week: i32,
    pub platform: String,
    pub current_episode: i32,
    pub episode_offset: i32,
    pub auto_complete: bool,
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
                series_id, mikan_id, bgmtv_id,
                title_chinese, title_japanese, season, year,
                total_episodes, poster_url, air_date, air_week, platform,
                current_episode, episode_offset, auto_complete, source_type
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)
            RETURNING id
            "#,
        )
        .bind(data.series_id)
        .bind(&data.mikan_id)
        .bind(data.bgmtv_id)
        .bind(&data.title_chinese)
        .bind(&data.title_japanese)
        .bind(data.season)
        .bind(data.year)
        .bind(data.total_episodes)
        .bind(&data.poster_url)
        .bind(&data.air_date)
        .bind(data.air_week)
        .bind(&data.platform)
        .bind(data.current_episode)
        .bind(data.episode_offset)
        .bind(data.auto_complete)
        .bind(&data.source_type)
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
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

    /// Get a bangumi by Mikan ID
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

    /// Get a bangumi by BGM.tv ID
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

    /// Get a bangumi by series_id
    pub async fn get_by_series_id(
        pool: &SqlitePool,
        series_id: i64,
    ) -> Result<Vec<Bangumi>, sqlx::Error> {
        let query = format!("{} WHERE series_id = $1 ORDER BY season ASC", SELECT_BANGUMI);
        let rows = sqlx::query_as::<_, BangumiRow>(&query)
            .bind(series_id)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get bangumi with series by ID
    pub async fn get_with_series_by_id(
        pool: &SqlitePool,
        id: i64,
    ) -> Result<Option<BangumiWithSeries>, sqlx::Error> {
        let query = format!("{} WHERE b.id = $1", SELECT_BANGUMI_WITH_SERIES);
        let row = sqlx::query_as::<_, BangumiWithSeriesRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get all bangumi
    pub async fn get_all(pool: &SqlitePool) -> Result<Vec<Bangumi>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC", SELECT_BANGUMI);
        let rows = sqlx::query_as::<_, BangumiRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get all bangumi with series
    pub async fn get_all_with_series(
        pool: &SqlitePool,
    ) -> Result<Vec<BangumiWithSeries>, sqlx::Error> {
        let query = format!("{} ORDER BY b.created_at DESC", SELECT_BANGUMI_WITH_SERIES);
        let rows = sqlx::query_as::<_, BangumiWithSeriesRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Update a bangumi
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateBangumi,
    ) -> Result<Option<Bangumi>, sqlx::Error> {
        let mikan_id_update = data.mikan_id.should_update();
        let bgmtv_id_update = data.bgmtv_id.should_update();
        let title_japanese_update = data.title_japanese.should_update();
        let poster_url_update = data.poster_url.should_update();
        let air_date_update = data.air_date.should_update();

        let result = sqlx::query(
            r#"
            UPDATE bangumi SET
                mikan_id = CASE WHEN $1 THEN $2 ELSE mikan_id END,
                bgmtv_id = CASE WHEN $3 THEN $4 ELSE bgmtv_id END,
                title_chinese = COALESCE($5, title_chinese),
                title_japanese = CASE WHEN $6 THEN $7 ELSE title_japanese END,
                season = COALESCE($8, season),
                year = COALESCE($9, year),
                total_episodes = COALESCE($10, total_episodes),
                poster_url = CASE WHEN $11 THEN $12 ELSE poster_url END,
                air_date = CASE WHEN $13 THEN $14 ELSE air_date END,
                air_week = COALESCE($15, air_week),
                platform = COALESCE($16, platform),
                current_episode = COALESCE($17, current_episode),
                episode_offset = COALESCE($18, episode_offset),
                auto_complete = COALESCE($19, auto_complete),
                source_type = COALESCE($20, source_type)
            WHERE id = $21
            "#,
        )
        .bind(mikan_id_update)
        .bind(data.mikan_id.into_value())
        .bind(bgmtv_id_update)
        .bind(data.bgmtv_id.into_value())
        .bind(&data.title_chinese)
        .bind(title_japanese_update)
        .bind(data.title_japanese.into_value())
        .bind(data.season)
        .bind(data.year)
        .bind(data.total_episodes)
        .bind(poster_url_update)
        .bind(data.poster_url.into_value())
        .bind(air_date_update)
        .bind(data.air_date.into_value())
        .bind(data.air_week)
        .bind(data.platform.map(|p| p.as_str().to_string()))
        .bind(data.current_episode)
        .bind(data.episode_offset)
        .bind(data.auto_complete)
        .bind(data.source_type.map(|s| s.as_str().to_string()))
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

    /// Check if Mikan ID exists
    pub async fn mikan_id_exists(pool: &SqlitePool, mikan_id: &str) -> Result<bool, sqlx::Error> {
        let count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM bangumi WHERE mikan_id = $1")
            .bind(mikan_id)
            .fetch_one(pool)
            .await?;
        Ok(count.0 > 0)
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

    /// Get bangumi records that need poster syncing (remote poster URL)
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

    /// Count bangumi records that need poster syncing
    pub async fn count_bangumi_to_sync(pool: &SqlitePool) -> Result<i64, sqlx::Error> {
        let result: (i64,) = sqlx::query_as(
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

        Ok(result.0)
    }

    /// Batch update poster URLs for multiple bangumi
    pub async fn batch_update_poster_urls(
        pool: &SqlitePool,
        updates: &[(i64, String)],
    ) -> Result<u64, sqlx::Error> {
        if updates.is_empty() {
            return Ok(0);
        }

        let mut affected = 0u64;
        for (id, poster_url) in updates {
            let result = sqlx::query("UPDATE bangumi SET poster_url = $1 WHERE id = $2")
                .bind(poster_url)
                .bind(id)
                .execute(pool)
                .await?;
            affected += result.rows_affected();
        }

        Ok(affected)
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
struct BangumiRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    series_id: i64,
    mikan_id: Option<String>,
    bgmtv_id: Option<i64>,
    title_chinese: String,
    title_japanese: Option<String>,
    season: i32,
    year: i32,
    total_episodes: i32,
    poster_url: Option<String>,
    air_date: Option<String>,
    air_week: i32,
    platform: String,
    current_episode: i32,
    episode_offset: i32,
    auto_complete: bool,
    source_type: String,
}

impl From<BangumiRow> for Bangumi {
    fn from(row: BangumiRow) -> Self {
        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            series_id: row.series_id,
            mikan_id: row.mikan_id,
            bgmtv_id: row.bgmtv_id,
            title_chinese: row.title_chinese,
            title_japanese: row.title_japanese,
            season: row.season,
            year: row.year,
            total_episodes: row.total_episodes,
            poster_url: row.poster_url,
            air_date: row.air_date,
            air_week: row.air_week,
            platform: row.platform.parse().unwrap_or_default(),
            current_episode: row.current_episode,
            episode_offset: row.episode_offset,
            auto_complete: row.auto_complete,
            source_type: row.source_type.parse().unwrap_or_default(),
        }
    }
}

/// Internal row type for bangumi with series JOIN
#[derive(Debug, sqlx::FromRow)]
struct BangumiWithSeriesRow {
    // Bangumi fields
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    series_id: i64,
    mikan_id: Option<String>,
    bgmtv_id: Option<i64>,
    title_chinese: String,
    title_japanese: Option<String>,
    season: i32,
    year: i32,
    total_episodes: i32,
    poster_url: Option<String>,
    air_date: Option<String>,
    air_week: i32,
    platform: String,
    current_episode: i32,
    episode_offset: i32,
    auto_complete: bool,
    source_type: String,
    // Series fields (prefixed with s_)
    s_id: i64,
    s_created_at: DateTime<Utc>,
    s_updated_at: DateTime<Utc>,
    s_tmdb_id: Option<i64>,
    s_title_chinese: String,
    s_title_japanese: Option<String>,
    s_poster_url: Option<String>,
}

impl From<BangumiWithSeriesRow> for BangumiWithSeries {
    fn from(row: BangumiWithSeriesRow) -> Self {
        Self {
            bangumi: Bangumi {
                id: row.id,
                created_at: row.created_at,
                updated_at: row.updated_at,
                series_id: row.series_id,
                mikan_id: row.mikan_id,
                bgmtv_id: row.bgmtv_id,
                title_chinese: row.title_chinese,
                title_japanese: row.title_japanese,
                season: row.season,
                year: row.year,
                total_episodes: row.total_episodes,
                poster_url: row.poster_url,
                air_date: row.air_date,
                air_week: row.air_week,
                platform: row.platform.parse().unwrap_or_default(),
                current_episode: row.current_episode,
                episode_offset: row.episode_offset,
                auto_complete: row.auto_complete,
                source_type: row.source_type.parse().unwrap_or_default(),
            },
            series: Series {
                id: row.s_id,
                created_at: row.s_created_at,
                updated_at: row.s_updated_at,
                tmdb_id: row.s_tmdb_id,
                title_chinese: row.s_title_chinese,
                title_japanese: row.s_title_japanese,
                poster_url: row.s_poster_url,
            },
        }
    }
}
