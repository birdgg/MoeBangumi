use std::collections::{HashMap, HashSet};

use chrono::{DateTime, Utc};
use parser::SubType;
use sqlx::{Executor, Sqlite, SqlitePool};

use crate::models::{CreateTorrent, Torrent, UpdateTorrent};

/// Common SELECT fields for torrent queries
const SELECT_TORRENT: &str = r#"
    SELECT
        id, created_at, updated_at,
        bangumi_id, rss_id, info_hash, torrent_url, episode_number,
        subtitle_group, subtitle_languages, resolution
    FROM torrent
"#;

pub struct TorrentRepository;

impl TorrentRepository {
    /// Create a new torrent
    pub async fn create(pool: &SqlitePool, data: CreateTorrent) -> Result<Torrent, sqlx::Error> {
        let id = Self::create_with_executor(pool, data).await?;
        Self::get_by_id(pool, id)
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
        // Serialize subtitle_languages to JSON string for storage
        let subtitle_languages_json = if data.subtitle_languages.is_empty() {
            None
        } else {
            match serde_json::to_string(&data.subtitle_languages) {
                Ok(json) => Some(json),
                Err(e) => {
                    tracing::error!(
                        "Failed to serialize subtitle_languages for torrent {}: {}",
                        data.info_hash,
                        e
                    );
                    None
                }
            }
        };

        let result = sqlx::query(
            r#"
            INSERT INTO torrent (bangumi_id, rss_id, info_hash, torrent_url, episode_number, subtitle_group, subtitle_languages, resolution)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
            RETURNING id
            "#,
        )
        .bind(data.bangumi_id)
        .bind(data.rss_id)
        .bind(&data.info_hash)
        .bind(&data.torrent_url)
        .bind(data.episode_number)
        .bind(&data.subtitle_group)
        .bind(&subtitle_languages_json)
        .bind(&data.resolution)
        .fetch_one(executor)
        .await?;

        Ok(sqlx::Row::get(&result, "id"))
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
    /// Uses CASE/COALESCE for atomic update to avoid read-modify-write race conditions
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateTorrent,
    ) -> Result<Option<Torrent>, sqlx::Error> {
        // Extract should_update flags for Clearable fields
        let rss_id_update = data.rss_id.should_update();
        let episode_number_update = data.episode_number.should_update();

        let result = sqlx::query(
            r#"
            UPDATE torrent SET
                rss_id = CASE WHEN $1 THEN $2 ELSE rss_id END,
                torrent_url = COALESCE($3, torrent_url),
                episode_number = CASE WHEN $4 THEN $5 ELSE episode_number END
            WHERE id = $6
            "#,
        )
        .bind(rss_id_update)
        .bind(data.rss_id.into_value())
        .bind(&data.torrent_url)
        .bind(episode_number_update)
        .bind(data.episode_number.into_value())
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

    /// Batch get torrents by episode numbers for a specific bangumi
    /// Returns a HashMap of episode_number -> Vec<Torrent> for efficient lookup
    pub async fn get_by_episodes(
        pool: &SqlitePool,
        bangumi_id: i64,
        episode_numbers: &[i32],
    ) -> Result<HashMap<i32, Vec<Torrent>>, sqlx::Error> {
        if episode_numbers.is_empty() {
            return Ok(HashMap::new());
        }

        // SQLite has a limit on bind parameters (default 999), so we chunk the input
        // Reserve 1 parameter for bangumi_id, use up to 499 for episode numbers
        const CHUNK_SIZE: usize = 499;
        let mut result: HashMap<i32, Vec<Torrent>> = HashMap::new();

        for chunk in episode_numbers.chunks(CHUNK_SIZE) {
            let placeholders: Vec<String> = (2..=chunk.len() + 1)
                .map(|i| format!("${}", i))
                .collect();
            let query = format!(
                "{} WHERE bangumi_id = $1 AND episode_number IN ({}) ORDER BY episode_number ASC",
                SELECT_TORRENT,
                placeholders.join(", ")
            );

            let mut query_builder = sqlx::query_as::<_, TorrentRow>(&query).bind(bangumi_id);
            for ep in chunk {
                query_builder = query_builder.bind(ep);
            }

            let rows: Vec<TorrentRow> = query_builder.fetch_all(pool).await?;

            // Group by episode number
            for row in rows {
                let torrent: Torrent = row.into();
                if let Some(ep) = torrent.episode_number {
                    result.entry(ep).or_default().push(torrent);
                }
            }
        }

        Ok(result)
    }

    /// Get all torrent info_hashes with their torrent_url for sync purposes
    /// Returns tuples of (info_hash, torrent_url, save_path from bangumi)
    pub async fn get_all_for_sync(
        pool: &SqlitePool,
    ) -> Result<Vec<TorrentSyncInfo>, sqlx::Error> {
        let rows = sqlx::query_as::<_, TorrentSyncInfo>(
            r#"
            SELECT t.info_hash, t.torrent_url, b.save_path
            FROM torrent t
            JOIN bangumi b ON t.bangumi_id = b.id
            "#,
        )
        .fetch_all(pool)
        .await?;

        Ok(rows)
    }

    /// Get the best torrent for each episode of a bangumi
    /// "Best" means the first torrent found for each episode (by created_at DESC)
    pub async fn get_best_by_bangumi(
        pool: &SqlitePool,
        bangumi_id: i64,
    ) -> Result<HashMap<i32, Torrent>, sqlx::Error> {
        let query = format!(
            "{} WHERE bangumi_id = $1 AND episode_number IS NOT NULL ORDER BY episode_number ASC, created_at DESC",
            SELECT_TORRENT
        );
        let rows = sqlx::query_as::<_, TorrentRow>(&query)
            .bind(bangumi_id)
            .fetch_all(pool)
            .await?;

        // Keep only the first (best) torrent for each episode
        let mut result: HashMap<i32, Torrent> = HashMap::new();
        for row in rows {
            let torrent: Torrent = row.into();
            if let Some(ep) = torrent.episode_number {
                // Only insert if not already present (first one is best due to ORDER BY)
                result.entry(ep).or_insert(torrent);
            }
        }

        Ok(result)
    }
}

/// Info needed for torrent sync job
#[derive(Debug, sqlx::FromRow)]
pub struct TorrentSyncInfo {
    pub info_hash: String,
    pub torrent_url: String,
    pub save_path: String,
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
    episode_number: Option<i32>,
    subtitle_group: Option<String>,
    subtitle_languages: Option<String>,
    resolution: Option<String>,
}

impl From<TorrentRow> for Torrent {
    fn from(row: TorrentRow) -> Self {
        // Parse subtitle_languages from JSON, falling back to empty vec
        let subtitle_languages = row
            .subtitle_languages
            .as_ref()
            .and_then(|s| {
                serde_json::from_str::<Vec<SubType>>(s)
                    .map_err(|e| {
                        tracing::warn!(
                            "Failed to parse subtitle_languages for torrent id={}: {} (raw: {:?})",
                            row.id,
                            e,
                            s
                        );
                        e
                    })
                    .ok()
            })
            .unwrap_or_default();

        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            bangumi_id: row.bangumi_id,
            rss_id: row.rss_id,
            info_hash: row.info_hash,
            torrent_url: row.torrent_url,
            episode_number: row.episode_number,
            subtitle_group: row.subtitle_group,
            subtitle_languages,
            resolution: row.resolution,
        }
    }
}
