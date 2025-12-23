use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{Bangumi, CreateBangumi, UpdateBangumi};

/// Common SELECT fields for bangumi queries
const SELECT_BANGUMI: &str = r#"
    SELECT
        id, created_at, updated_at,
        title_chinese, title_japanese, season, year,
        bgmtv_id, tmdb_id, poster_url, air_date,
        total_episodes, episode_offset, current_episode,
        auto_download, save_path, source_type
    FROM bangumi
"#;

pub struct BangumiRepository;

impl BangumiRepository {
    /// Create a new bangumi
    pub async fn create(pool: &SqlitePool, data: CreateBangumi) -> Result<Bangumi, sqlx::Error> {
        let result = sqlx::query(
            r#"
            INSERT INTO bangumi (
                title_chinese, title_japanese, season, year,
                bgmtv_id, tmdb_id, poster_url, air_date,
                total_episodes, episode_offset, auto_download,
                save_path, source_type
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
            RETURNING id
            "#,
        )
        .bind(&data.title_chinese)
        .bind(&data.title_japanese)
        .bind(data.season)
        .bind(data.year)
        .bind(data.bgmtv_id)
        .bind(data.tmdb_id)
        .bind(&data.poster_url)
        .bind(&data.air_date)
        .bind(data.total_episodes)
        .bind(data.episode_offset)
        .bind(data.auto_download)
        .bind(&data.save_path)
        .bind(data.source_type.as_str())
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

    /// Get all bangumi
    pub async fn get_all(pool: &SqlitePool) -> Result<Vec<Bangumi>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC", SELECT_BANGUMI);
        let rows = sqlx::query_as::<_, BangumiRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get bangumi with auto_download enabled
    pub async fn get_auto_download(pool: &SqlitePool) -> Result<Vec<Bangumi>, sqlx::Error> {
        let query = format!(
            "{} WHERE auto_download = 1 ORDER BY created_at DESC",
            SELECT_BANGUMI
        );
        let rows = sqlx::query_as::<_, BangumiRow>(&query)
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
        let existing = Self::get_by_id(pool, id).await?;
        let Some(existing) = existing else {
            return Ok(None);
        };

        let title_chinese = data.title_chinese.unwrap_or(existing.title_chinese);
        let title_japanese = data.title_japanese.resolve(existing.title_japanese);
        let season = data.season.unwrap_or(existing.season);
        let year = data.year.unwrap_or(existing.year);
        let bgmtv_id = data.bgmtv_id.resolve(existing.bgmtv_id);
        let tmdb_id = data.tmdb_id.resolve(existing.tmdb_id);
        let poster_url = data.poster_url.resolve(existing.poster_url);
        let air_date = data.air_date.resolve(existing.air_date);
        let total_episodes = data.total_episodes.unwrap_or(existing.total_episodes);
        let episode_offset = data.episode_offset.unwrap_or(existing.episode_offset);
        let current_episode = data.current_episode.unwrap_or(existing.current_episode);
        let auto_download = data.auto_download.unwrap_or(existing.auto_download);
        let save_path = data.save_path.resolve(existing.save_path);
        let source_type = data.source_type.unwrap_or(existing.source_type);

        sqlx::query(
            r#"
            UPDATE bangumi SET
                title_chinese = $1,
                title_japanese = $2,
                season = $3,
                year = $4,
                bgmtv_id = $5,
                tmdb_id = $6,
                poster_url = $7,
                air_date = $8,
                total_episodes = $9,
                episode_offset = $10,
                current_episode = $11,
                auto_download = $12,
                save_path = $13,
                source_type = $14
            WHERE id = $15
            "#,
        )
        .bind(&title_chinese)
        .bind(&title_japanese)
        .bind(season)
        .bind(year)
        .bind(bgmtv_id)
        .bind(tmdb_id)
        .bind(&poster_url)
        .bind(&air_date)
        .bind(total_episodes)
        .bind(episode_offset)
        .bind(current_episode)
        .bind(auto_download)
        .bind(&save_path)
        .bind(source_type.as_str())
        .bind(id)
        .execute(pool)
        .await?;

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
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct BangumiRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    title_chinese: String,
    title_japanese: Option<String>,
    season: i32,
    year: i32,
    bgmtv_id: Option<i64>,
    tmdb_id: Option<i64>,
    poster_url: Option<String>,
    air_date: Option<String>,
    total_episodes: i32,
    episode_offset: i32,
    current_episode: i32,
    auto_download: bool,
    save_path: Option<String>,
    source_type: String,
}

impl From<BangumiRow> for Bangumi {
    fn from(row: BangumiRow) -> Self {
        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            title_chinese: row.title_chinese,
            title_japanese: row.title_japanese,
            season: row.season,
            year: row.year,
            bgmtv_id: row.bgmtv_id,
            tmdb_id: row.tmdb_id,
            poster_url: row.poster_url,
            air_date: row.air_date,
            total_episodes: row.total_episodes,
            episode_offset: row.episode_offset,
            current_episode: row.current_episode,
            auto_download: row.auto_download,
            save_path: row.save_path,
            source_type: row.source_type.parse().unwrap_or_default(),
        }
    }
}
