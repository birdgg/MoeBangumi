use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateTorrent, Torrent, TorrentKind, UpdateTorrent};

/// Common SELECT fields for torrent queries
const SELECT_TORRENT: &str = r#"
    SELECT
        id, created_at, updated_at,
        bangumi_id, rss_id, info_hash, torrent_url, kind, episode_number
    FROM torrent
"#;

pub struct TorrentRepository;

impl TorrentRepository {
    /// Create a new torrent
    pub async fn create(pool: &SqlitePool, data: CreateTorrent) -> Result<Torrent, sqlx::Error> {
        let result = sqlx::query(
            r#"
            INSERT INTO torrent (bangumi_id, rss_id, info_hash, torrent_url, kind, episode_number)
            VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING id
            "#,
        )
        .bind(data.bangumi_id)
        .bind(data.rss_id)
        .bind(&data.info_hash)
        .bind(&data.torrent_url)
        .bind(data.kind.as_str())
        .bind(data.episode_number)
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Get a torrent by ID
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Torrent>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_TORRENT);
        let row = sqlx::query_as::<_, TorrentRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get a torrent by info_hash
    pub async fn get_by_info_hash(
        pool: &SqlitePool,
        info_hash: &str,
    ) -> Result<Option<Torrent>, sqlx::Error> {
        let query = format!("{} WHERE info_hash = $1", SELECT_TORRENT);
        let row = sqlx::query_as::<_, TorrentRow>(&query)
            .bind(info_hash)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get all torrents for a bangumi
    pub async fn get_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<Vec<Torrent>, sqlx::Error> {
        let query = format!(
            "{} WHERE bangumi_id = $1 ORDER BY episode_number ASC NULLS LAST",
            SELECT_TORRENT
        );
        let rows = sqlx::query_as::<_, TorrentRow>(&query)
            .bind(bangumi_id)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get torrents for a specific bangumi episode
    pub async fn get_by_bangumi_episode(
        pool: &SqlitePool,
        bangumi_id: i64,
        episode_number: i32,
    ) -> Result<Vec<Torrent>, sqlx::Error> {
        let query = format!(
            "{} WHERE bangumi_id = $1 AND episode_number = $2 ORDER BY created_at DESC",
            SELECT_TORRENT
        );
        let rows = sqlx::query_as::<_, TorrentRow>(&query)
            .bind(bangumi_id)
            .bind(episode_number)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get all torrents from a specific RSS source
    pub async fn get_by_rss_id(
        pool: &SqlitePool,
        rss_id: i64,
    ) -> Result<Vec<Torrent>, sqlx::Error> {
        let query = format!(
            "{} WHERE rss_id = $1 ORDER BY episode_number ASC NULLS LAST",
            SELECT_TORRENT
        );
        let rows = sqlx::query_as::<_, TorrentRow>(&query)
            .bind(rss_id)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Update a torrent
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateTorrent,
    ) -> Result<Option<Torrent>, sqlx::Error> {
        let existing = Self::get_by_id(pool, id).await?;
        let Some(existing) = existing else {
            return Ok(None);
        };

        let rss_id = data.rss_id.resolve(existing.rss_id);
        let torrent_url = data.torrent_url.unwrap_or(existing.torrent_url);
        let kind = data.kind.unwrap_or(existing.kind);
        let episode_number = data.episode_number.resolve(existing.episode_number);

        sqlx::query(
            r#"
            UPDATE torrent SET
                rss_id = $1,
                torrent_url = $2,
                kind = $3,
                episode_number = $4
            WHERE id = $5
            "#,
        )
        .bind(rss_id)
        .bind(torrent_url)
        .bind(kind.as_str())
        .bind(episode_number)
        .bind(id)
        .execute(pool)
        .await?;

        Self::get_by_id(pool, id).await
    }

    /// Delete a torrent by ID
    pub async fn delete(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("DELETE FROM torrent WHERE id = $1")
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Delete all torrents for a bangumi
    pub async fn delete_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<u64, sqlx::Error> {
        let result = sqlx::query("DELETE FROM torrent WHERE bangumi_id = $1")
            .bind(bangumi_id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected())
    }

    /// Check if a torrent with the given info_hash exists
    pub async fn exists_by_info_hash(
        pool: &SqlitePool,
        info_hash: &str,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query_scalar::<_, i64>(
            "SELECT COUNT(*) FROM torrent WHERE info_hash = $1",
        )
        .bind(info_hash)
        .fetch_one(pool)
        .await?;

        Ok(result > 0)
    }

    /// Check if a torrent for the given bangumi and episode already exists
    pub async fn exists_by_bangumi_episode(
        pool: &SqlitePool,
        bangumi_id: i64,
        episode_number: i32,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query_scalar::<_, i64>(
            "SELECT COUNT(*) FROM torrent WHERE bangumi_id = $1 AND episode_number = $2",
        )
        .bind(bangumi_id)
        .bind(episode_number)
        .fetch_one(pool)
        .await?;

        Ok(result > 0)
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct TorrentRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    bangumi_id: i64,
    rss_id: Option<i64>,
    info_hash: String,
    torrent_url: String,
    kind: String,
    episode_number: Option<i32>,
}

impl From<TorrentRow> for Torrent {
    fn from(row: TorrentRow) -> Self {
        let kind = row.kind.parse().unwrap_or_else(|_| {
            tracing::warn!(
                "Invalid torrent kind '{}' for torrent id {}, defaulting to Episode",
                row.kind,
                row.id
            );
            TorrentKind::Episode
        });

        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            bangumi_id: row.bangumi_id,
            rss_id: row.rss_id,
            info_hash: row.info_hash,
            torrent_url: row.torrent_url,
            kind,
            episode_number: row.episode_number,
        }
    }
}
