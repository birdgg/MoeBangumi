use sqlx::SqlitePool;

/// Common SELECT fields for mikan mapping queries
const SELECT_MAPPING: &str = r#"
    SELECT mikan_id, bgmtv_id, tmdb_id, created_at
    FROM mikan_bgmtv_mapping
"#;

pub struct MikanMappingRepository;

impl MikanMappingRepository {
    /// Get mapping by BGM.tv ID
    pub async fn get_by_bgmtv_id(
        pool: &SqlitePool,
        bgmtv_id: i64,
    ) -> Result<Option<MikanMappingRow>, sqlx::Error> {
        let query = format!("{} WHERE bgmtv_id = $1", SELECT_MAPPING);
        sqlx::query_as::<_, MikanMappingRow>(&query)
            .bind(bgmtv_id)
            .fetch_optional(pool)
            .await
    }

    /// Get mapping by TMDB ID
    pub async fn get_by_tmdb_id(
        pool: &SqlitePool,
        tmdb_id: i64,
    ) -> Result<Option<MikanMappingRow>, sqlx::Error> {
        let query = format!("{} WHERE tmdb_id = $1", SELECT_MAPPING);
        sqlx::query_as::<_, MikanMappingRow>(&query)
            .bind(tmdb_id)
            .fetch_optional(pool)
            .await
    }

    /// Get mapping by Mikan ID
    pub async fn get_by_mikan_id(
        pool: &SqlitePool,
        mikan_id: &str,
    ) -> Result<Option<MikanMappingRow>, sqlx::Error> {
        let query = format!("{} WHERE mikan_id = $1", SELECT_MAPPING);
        sqlx::query_as::<_, MikanMappingRow>(&query)
            .bind(mikan_id)
            .fetch_optional(pool)
            .await
    }

    /// Check if Mikan ID exists
    pub async fn exists(pool: &SqlitePool, mikan_id: &str) -> Result<bool, sqlx::Error> {
        let count: (i64,) =
            sqlx::query_as("SELECT COUNT(*) FROM mikan_bgmtv_mapping WHERE mikan_id = $1")
                .bind(mikan_id)
                .fetch_one(pool)
                .await?;
        Ok(count.0 > 0)
    }

    /// Batch insert mappings (ignores existing records)
    pub async fn upsert_batch(
        pool: &SqlitePool,
        rows: &[MikanMappingRow],
    ) -> Result<usize, sqlx::Error> {
        let mut tx = pool.begin().await?;
        let mut inserted = 0;

        for row in rows {
            let result = sqlx::query(
                r#"
                INSERT OR IGNORE INTO mikan_bgmtv_mapping (mikan_id, bgmtv_id)
                VALUES ($1, $2)
                "#,
            )
            .bind(&row.mikan_id)
            .bind(row.bgmtv_id)
            .execute(&mut *tx)
            .await?;

            if result.rows_affected() > 0 {
                inserted += 1;
            }
        }

        tx.commit().await?;
        Ok(inserted)
    }

    /// Update TMDB ID for a mapping
    pub async fn update_tmdb_id(
        pool: &SqlitePool,
        mikan_id: &str,
        tmdb_id: i64,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query(
            "UPDATE mikan_bgmtv_mapping SET tmdb_id = $1 WHERE mikan_id = $2",
        )
        .bind(tmdb_id)
        .bind(mikan_id)
        .execute(pool)
        .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Get all mappings
    pub async fn get_all(pool: &SqlitePool) -> Result<Vec<MikanMappingRow>, sqlx::Error> {
        let query = format!("{} ORDER BY created_at DESC", SELECT_MAPPING);
        sqlx::query_as::<_, MikanMappingRow>(&query)
            .fetch_all(pool)
            .await
    }

    /// Count total mappings
    pub async fn count(pool: &SqlitePool) -> Result<i64, sqlx::Error> {
        let count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM mikan_bgmtv_mapping")
            .fetch_one(pool)
            .await?;
        Ok(count.0)
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, Clone, sqlx::FromRow)]
pub struct MikanMappingRow {
    pub mikan_id: String,
    pub bgmtv_id: i64,
    pub tmdb_id: Option<i64>,
    pub created_at: String,
}

impl MikanMappingRow {
    /// Create a new mapping row (for insertion)
    pub fn new(mikan_id: String, bgmtv_id: i64) -> Self {
        Self {
            mikan_id,
            bgmtv_id,
            tmdb_id: None,
            created_at: String::new(),
        }
    }
}
