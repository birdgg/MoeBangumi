use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{Bangumi, BangumiWithMetadata, Metadata, UpdateBangumi};

/// Common SELECT fields for bangumi queries
const SELECT_BANGUMI: &str = r#"
    SELECT
        id, created_at, updated_at,
        metadata_id, current_episode,
        auto_complete, save_path, source_type
    FROM bangumi
"#;

/// SELECT for bangumi with metadata JOIN
const SELECT_BANGUMI_WITH_METADATA: &str = r#"
    SELECT
        b.id, b.created_at, b.updated_at,
        b.metadata_id, b.current_episode,
        b.auto_complete, b.save_path, b.source_type,
        m.id as m_id, m.created_at as m_created_at, m.updated_at as m_updated_at,
        m.mikan_id, m.bgmtv_id, m.tmdb_id,
        m.title_chinese, m.title_japanese,
        m.season, m.year, m.platform,
        m.total_episodes, m.poster_url, m.air_date, m.air_week,
        m.tmdb_lookup_at as m_tmdb_lookup_at,
        m.episode_offset as m_episode_offset
    FROM bangumi b
    INNER JOIN metadata m ON b.metadata_id = m.id
"#;

/// Data required to create a new bangumi
pub struct CreateBangumiData {
    pub metadata_id: i64,
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
                metadata_id, auto_complete, save_path, source_type
            )
            VALUES ($1, $2, $3, $4)
            RETURNING id
            "#,
        )
        .bind(data.metadata_id)
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

    /// Get a bangumi by ID
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Bangumi>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_BANGUMI);
        let row = sqlx::query_as::<_, BangumiRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get a bangumi by metadata_id
    pub async fn get_by_metadata_id(
        pool: &SqlitePool,
        metadata_id: i64,
    ) -> Result<Option<Bangumi>, sqlx::Error> {
        let query = format!("{} WHERE metadata_id = $1", SELECT_BANGUMI);
        let row = sqlx::query_as::<_, BangumiRow>(&query)
            .bind(metadata_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get bangumi with metadata by ID
    pub async fn get_with_metadata_by_id(
        pool: &SqlitePool,
        id: i64,
    ) -> Result<Option<BangumiWithMetadata>, sqlx::Error> {
        let query = format!("{} WHERE b.id = $1", SELECT_BANGUMI_WITH_METADATA);
        let row = sqlx::query_as::<_, BangumiWithMetadataRow>(&query)
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

    /// Get all bangumi with metadata
    pub async fn get_all_with_metadata(
        pool: &SqlitePool,
    ) -> Result<Vec<BangumiWithMetadata>, sqlx::Error> {
        let query = format!("{} ORDER BY b.created_at DESC", SELECT_BANGUMI_WITH_METADATA);
        let rows = sqlx::query_as::<_, BangumiWithMetadataRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Update a bangumi
    /// Uses COALESCE for atomic update to avoid read-modify-write race conditions
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateBangumi,
    ) -> Result<Option<Bangumi>, sqlx::Error> {
        let result = sqlx::query(
            r#"
            UPDATE bangumi SET
                current_episode = COALESCE($1, current_episode),
                auto_complete = COALESCE($2, auto_complete),
                source_type = COALESCE($3, source_type)
            WHERE id = $4
            "#,
        )
        .bind(data.current_episode)
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
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct BangumiRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    metadata_id: i64,
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
            metadata_id: row.metadata_id,
            current_episode: row.current_episode,
            auto_complete: row.auto_complete,
            save_path: row.save_path,
            source_type: row.source_type.parse().unwrap_or_default(),
        }
    }
}

/// Internal row type for bangumi with metadata JOIN
#[derive(Debug, sqlx::FromRow)]
struct BangumiWithMetadataRow {
    // Bangumi fields
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    metadata_id: i64,
    current_episode: i32,
    auto_complete: bool,
    save_path: String,
    source_type: String,
    // Metadata fields (prefixed with m_)
    m_id: i64,
    m_created_at: DateTime<Utc>,
    m_updated_at: DateTime<Utc>,
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
    m_tmdb_lookup_at: Option<DateTime<Utc>>,
    m_episode_offset: i32,
}

impl From<BangumiWithMetadataRow> for BangumiWithMetadata {
    fn from(row: BangumiWithMetadataRow) -> Self {
        Self {
            bangumi: Bangumi {
                id: row.id,
                created_at: row.created_at,
                updated_at: row.updated_at,
                metadata_id: row.metadata_id,
                current_episode: row.current_episode,
                auto_complete: row.auto_complete,
                save_path: row.save_path,
                source_type: row.source_type.parse().unwrap_or_default(),
            },
            metadata: Metadata {
                id: row.m_id,
                created_at: row.m_created_at,
                updated_at: row.m_updated_at,
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
                tmdb_lookup_at: row.m_tmdb_lookup_at,
                episode_offset: row.m_episode_offset,
            },
        }
    }
}
