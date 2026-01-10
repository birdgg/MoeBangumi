use std::collections::HashSet;

use chrono::{DateTime, Utc};
use sqlx::{Executor, Sqlite, SqlitePool};

use crate::models::{CreateTorrent, Torrent, TorrentWithBangumi, UpdateTorrent};

/// Common SELECT fields for torrent queries
const SELECT_TORRENT: &str = r#"
    SELECT
        id, created_at, updated_at,
        rss_id, info_hash, torrent_url
    FROM torrent
"#;

pub struct TorrentRepository;

impl TorrentRepository {
    /// Create a new torrent with bangumi associations
    pub async fn create(pool: &SqlitePool, data: CreateTorrent) -> Result<Torrent, sqlx::Error> {
        let mut tx = pool.begin().await?;

        // Insert torrent
        let result = sqlx::query(
            r#"
            INSERT INTO torrent (rss_id, info_hash, torrent_url)
            VALUES ($1, $2, $3)
            RETURNING id
            "#,
        )
        .bind(data.rss_id)
        .bind(&data.info_hash)
        .bind(&data.torrent_url)
        .fetch_one(&mut *tx)
        .await?;

        let torrent_id: i64 = sqlx::Row::get(&result, "id");

        // Insert bangumi associations
        for bangumi_id in &data.bangumi_ids {
            sqlx::query(
                "INSERT INTO torrent_bangumi (torrent_id, bangumi_id) VALUES ($1, $2)",
            )
            .bind(torrent_id)
            .bind(bangumi_id)
            .execute(&mut *tx)
            .await?;
        }

        tx.commit().await?;

        Self::get_by_id(pool, torrent_id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Create a new torrent using a generic executor (supports transactions)
    /// Returns the created torrent ID
    pub async fn create_with_executor<'e, E>(
        executor: E,
        data: CreateTorrent,
    ) -> Result<i64, sqlx::Error>
    where
        E: Executor<'e, Database = Sqlite>,
    {
        let result = sqlx::query(
            r#"
            INSERT INTO torrent (rss_id, info_hash, torrent_url)
            VALUES ($1, $2, $3)
            RETURNING id
            "#,
        )
        .bind(data.rss_id)
        .bind(&data.info_hash)
        .bind(&data.torrent_url)
        .fetch_one(executor)
        .await?;

        Ok(sqlx::Row::get(&result, "id"))
    }

    /// Add bangumi associations for a torrent
    pub async fn add_bangumi_associations(
        pool: &SqlitePool,
        torrent_id: i64,
        bangumi_ids: &[i64],
    ) -> Result<(), sqlx::Error> {
        for bangumi_id in bangumi_ids {
            sqlx::query(
                "INSERT OR IGNORE INTO torrent_bangumi (torrent_id, bangumi_id) VALUES ($1, $2)",
            )
            .bind(torrent_id)
            .bind(bangumi_id)
            .execute(pool)
            .await?;
        }
        Ok(())
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

    /// Get a torrent with its associated bangumi IDs
    pub async fn get_with_bangumi_by_id(
        pool: &SqlitePool,
        id: i64,
    ) -> Result<Option<TorrentWithBangumi>, sqlx::Error> {
        let torrent = Self::get_by_id(pool, id).await?;

        match torrent {
            Some(t) => {
                let bangumi_ids = Self::get_bangumi_ids(pool, id).await?;
                Ok(Some(TorrentWithBangumi {
                    torrent: t,
                    bangumi_ids,
                }))
            }
            None => Ok(None),
        }
    }

    /// Get bangumi IDs for a torrent
    pub async fn get_bangumi_ids(
        pool: &SqlitePool,
        torrent_id: i64,
    ) -> Result<Vec<i64>, sqlx::Error> {
        let rows: Vec<(i64,)> = sqlx::query_as(
            "SELECT bangumi_id FROM torrent_bangumi WHERE torrent_id = $1",
        )
        .bind(torrent_id)
        .fetch_all(pool)
        .await?;

        Ok(rows.into_iter().map(|(id,)| id).collect())
    }

    /// Get all torrents for a bangumi
    pub async fn get_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<Vec<Torrent>, sqlx::Error> {
        let rows = sqlx::query_as::<_, TorrentRow>(
            r#"
            SELECT t.id, t.created_at, t.updated_at, t.rss_id, t.info_hash, t.torrent_url
            FROM torrent t
            INNER JOIN torrent_bangumi tb ON t.id = tb.torrent_id
            WHERE tb.bangumi_id = $1
            ORDER BY t.created_at DESC
            "#,
        )
        .bind(bangumi_id)
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
            "{} WHERE rss_id = $1 ORDER BY created_at DESC",
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
        let result = sqlx::query(
            r#"
            UPDATE torrent SET
                torrent_url = COALESCE($1, torrent_url)
            WHERE id = $2
            "#,
        )
        .bind(&data.torrent_url)
        .bind(id)
        .execute(pool)
        .await?;

        if result.rows_affected() == 0 {
            return Ok(None);
        }

        Self::get_by_id(pool, id).await
    }

    /// Delete a torrent by ID
    pub async fn delete(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error> {
        Self::delete_with_executor(pool, id).await
    }

    /// Delete a torrent by ID using a generic executor (supports transactions)
    pub async fn delete_with_executor<'e, E>(executor: E, id: i64) -> Result<bool, sqlx::Error>
    where
        E: Executor<'e, Database = Sqlite>,
    {
        // torrent_bangumi entries are automatically deleted via CASCADE
        let result = sqlx::query("DELETE FROM torrent WHERE id = $1")
            .bind(id)
            .execute(executor)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Delete all torrents for a bangumi
    pub async fn delete_by_bangumi_id(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<u64, sqlx::Error> {
        // Get torrent IDs that are ONLY associated with this bangumi
        let exclusive_torrents: Vec<(i64,)> = sqlx::query_as(
            r#"
            SELECT tb.torrent_id
            FROM torrent_bangumi tb
            WHERE tb.bangumi_id = $1
            AND NOT EXISTS (
                SELECT 1 FROM torrent_bangumi tb2
                WHERE tb2.torrent_id = tb.torrent_id AND tb2.bangumi_id != $1
            )
            "#,
        )
        .bind(bangumi_id)
        .fetch_all(pool)
        .await?;

        let exclusive_ids: Vec<i64> = exclusive_torrents.into_iter().map(|(id,)| id).collect();

        // Delete associations first
        sqlx::query("DELETE FROM torrent_bangumi WHERE bangumi_id = $1")
            .bind(bangumi_id)
            .execute(pool)
            .await?;

        // Delete torrents that were exclusively associated with this bangumi
        if exclusive_ids.is_empty() {
            return Ok(0);
        }

        let placeholders: Vec<String> = (1..=exclusive_ids.len())
            .map(|i| format!("${}", i))
            .collect();
        let query = format!(
            "DELETE FROM torrent WHERE id IN ({})",
            placeholders.join(", ")
        );

        let mut q = sqlx::query(&query);
        for id in &exclusive_ids {
            q = q.bind(id);
        }

        let result = q.execute(pool).await?;
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

    /// Batch check which info_hashes already exist in the database
    /// Returns a HashSet of existing info_hashes for O(1) lookup
    pub async fn get_existing_hashes(
        pool: &SqlitePool,
        info_hashes: &[String],
    ) -> Result<HashSet<String>, sqlx::Error> {
        if info_hashes.is_empty() {
            return Ok(HashSet::new());
        }

        // SQLite has a limit on bind parameters (default 999), so we chunk the input
        const CHUNK_SIZE: usize = 500;
        let mut result = HashSet::new();

        for chunk in info_hashes.chunks(CHUNK_SIZE) {
            let placeholders: Vec<String> =
                (1..=chunk.len()).map(|i| format!("${}", i)).collect();
            let query = format!(
                "SELECT info_hash FROM torrent WHERE info_hash IN ({})",
                placeholders.join(", ")
            );

            let mut query_builder = sqlx::query_scalar::<_, String>(&query);
            for hash in chunk {
                query_builder = query_builder.bind(hash);
            }

            let existing: Vec<String> = query_builder.fetch_all(pool).await?;
            result.extend(existing);
        }

        Ok(result)
    }

    /// Get all torrent info for sync purposes
    /// Returns tuples of (info_hash, torrent_url)
    pub async fn get_all_for_sync(
        pool: &SqlitePool,
    ) -> Result<Vec<TorrentSyncInfo>, sqlx::Error> {
        let rows = sqlx::query_as::<_, TorrentSyncInfo>(
            r#"
            SELECT t.info_hash, t.torrent_url
            FROM torrent t
            "#,
        )
        .fetch_all(pool)
        .await?;

        Ok(rows)
    }
}

/// Info needed for torrent sync job
#[derive(Debug, sqlx::FromRow)]
pub struct TorrentSyncInfo {
    pub info_hash: String,
    pub torrent_url: String,
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct TorrentRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    rss_id: Option<i64>,
    info_hash: String,
    torrent_url: String,
}

impl From<TorrentRow> for Torrent {
    fn from(row: TorrentRow) -> Self {
        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            rss_id: row.rss_id,
            info_hash: row.info_hash,
            torrent_url: row.torrent_url,
        }
    }
}
