use sqlx::SqlitePool;

use crate::models::{CalendarSubject, Platform};

/// Calendar entry for batch insert
#[derive(Debug, Clone)]
pub struct CalendarEntry {
    pub metadata_id: i64,
    pub year: i32,
    pub season: String,
    pub priority: i64,
}

pub struct CalendarRepository;

impl CalendarRepository {
    /// Batch upsert calendar entries (transaction for atomicity)
    pub async fn upsert_batch(
        pool: &SqlitePool,
        entries: &[CalendarEntry],
    ) -> Result<usize, sqlx::Error> {
        let mut tx = pool.begin().await?;
        let mut count = 0;

        for entry in entries {
            let result = sqlx::query(
                r#"
                INSERT INTO calendar (metadata_id, year, season, priority)
                VALUES ($1, $2, $3, $4)
                ON CONFLICT(metadata_id, year, season) DO UPDATE SET
                    priority = excluded.priority,
                    updated_at = CURRENT_TIMESTAMP
                "#,
            )
            .bind(entry.metadata_id)
            .bind(entry.year)
            .bind(&entry.season)
            .bind(entry.priority)
            .execute(&mut *tx)
            .await?;

            count += result.rows_affected() as usize;
        }

        tx.commit().await?;
        Ok(count)
    }

    /// Get calendar entries with metadata for a specific season
    pub async fn get_by_season(
        pool: &SqlitePool,
        year: i32,
        season: &str,
    ) -> Result<Vec<CalendarWithMetadata>, sqlx::Error> {
        let rows = sqlx::query_as::<_, CalendarWithMetadataRow>(
            r#"
            SELECT
                c.id as calendar_id,
                c.metadata_id,
                c.year as calendar_year,
                c.season as calendar_season,
                c.priority,
                m.mikan_id,
                m.bgmtv_id,
                m.title_chinese,
                m.title_japanese,
                m.season as metadata_season,
                m.year as metadata_year,
                m.platform,
                m.total_episodes,
                m.poster_url,
                m.air_date,
                m.air_week
            FROM calendar c
            INNER JOIN metadata m ON c.metadata_id = m.id
            WHERE c.year = $1 AND c.season = $2
            ORDER BY c.priority DESC, m.air_week, m.title_chinese
            "#,
        )
        .bind(year)
        .bind(season)
        .fetch_all(pool)
        .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Check if any calendar data exists for a season
    pub async fn has_data(pool: &SqlitePool, year: i32, season: &str) -> Result<bool, sqlx::Error> {
        let count: (i64,) =
            sqlx::query_as("SELECT COUNT(*) FROM calendar WHERE year = $1 AND season = $2")
                .bind(year)
                .bind(season)
                .fetch_one(pool)
                .await?;
        Ok(count.0 > 0)
    }

    /// Check if calendar table is empty
    pub async fn is_empty(pool: &SqlitePool) -> Result<bool, sqlx::Error> {
        let count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM calendar")
            .fetch_one(pool)
            .await?;
        Ok(count.0 == 0)
    }

    /// Delete calendar entries for a specific season
    pub async fn delete_by_season(
        pool: &SqlitePool,
        year: i32,
        season: &str,
    ) -> Result<u64, sqlx::Error> {
        let result = sqlx::query("DELETE FROM calendar WHERE year = $1 AND season = $2")
            .bind(year)
            .bind(season)
            .execute(pool)
            .await?;

        Ok(result.rows_affected())
    }

    /// Delete calendar entries not in the provided list of metadata_ids (for a specific season)
    pub async fn delete_except(
        pool: &SqlitePool,
        year: i32,
        season: &str,
        keep_metadata_ids: &[i64],
    ) -> Result<u64, sqlx::Error> {
        if keep_metadata_ids.is_empty() {
            return Self::delete_by_season(pool, year, season).await;
        }

        let placeholders = keep_metadata_ids
            .iter()
            .enumerate()
            .map(|(i, _)| format!("${}", i + 3))
            .collect::<Vec<_>>()
            .join(",");

        let query = format!(
            "DELETE FROM calendar WHERE year = $1 AND season = $2 AND metadata_id NOT IN ({})",
            placeholders
        );

        let mut q = sqlx::query(&query).bind(year).bind(season);
        for id in keep_metadata_ids {
            q = q.bind(id);
        }

        let result = q.execute(pool).await?;
        Ok(result.rows_affected())
    }
}

/// Calendar entry with joined metadata
#[derive(Debug, Clone)]
pub struct CalendarWithMetadata {
    pub calendar_id: i64,
    pub metadata_id: i64,
    pub calendar_year: i32,
    pub calendar_season: String,
    pub priority: i64,
    pub mikan_id: Option<String>,
    pub bgmtv_id: Option<i64>,
    pub title_chinese: String,
    pub title_japanese: Option<String>,
    pub metadata_season: i32,
    pub metadata_year: i32,
    pub platform: String,
    pub total_episodes: i32,
    pub poster_url: Option<String>,
    pub air_date: Option<String>,
    pub air_week: i32,
}

impl CalendarWithMetadata {
    /// Convert air_week (0=Sunday, 1-6=Mon-Sat) to BGM.tv air_weekday (1-7=Mon-Sun)
    pub fn air_weekday(&self) -> i32 {
        match self.air_week {
            0 => 7,                 // Sunday -> 7
            1..=6 => self.air_week, // Monday-Saturday -> 1-6
            invalid => {
                tracing::warn!(
                    "Invalid air_week {} for metadata_id {}, defaulting to Monday",
                    invalid,
                    self.metadata_id
                );
                1 // fallback to Monday
            }
        }
    }

    /// Convert to CalendarSubject for API response
    pub fn to_calendar_subject(&self) -> CalendarSubject {
        let platform = match self.platform.as_str() {
            "movie" => Platform::Movie,
            "ova" => Platform::Ova,
            _ => Platform::Tv,
        };

        CalendarSubject {
            bgmtv_id: self.bgmtv_id,
            mikan_id: self.mikan_id.clone(),
            title_chinese: self.title_chinese.clone(),
            title_japanese: self.title_japanese.clone(),
            season: self.metadata_season,
            air_date: self.air_date.clone(),
            air_week: self.air_week,
            poster_url: self.poster_url.clone(),
            platform,
            total_episodes: self.total_episodes,
        }
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct CalendarWithMetadataRow {
    calendar_id: i64,
    metadata_id: i64,
    calendar_year: i32,
    calendar_season: String,
    priority: i64,
    mikan_id: Option<String>,
    bgmtv_id: Option<i64>,
    title_chinese: String,
    title_japanese: Option<String>,
    metadata_season: i32,
    metadata_year: i32,
    platform: String,
    total_episodes: i32,
    poster_url: Option<String>,
    air_date: Option<String>,
    air_week: i32,
}

impl From<CalendarWithMetadataRow> for CalendarWithMetadata {
    fn from(row: CalendarWithMetadataRow) -> Self {
        Self {
            calendar_id: row.calendar_id,
            metadata_id: row.metadata_id,
            calendar_year: row.calendar_year,
            calendar_season: row.calendar_season,
            priority: row.priority,
            mikan_id: row.mikan_id,
            bgmtv_id: row.bgmtv_id,
            title_chinese: row.title_chinese,
            title_japanese: row.title_japanese,
            metadata_season: row.metadata_season,
            metadata_year: row.metadata_year,
            platform: row.platform,
            total_episodes: row.total_episodes,
            poster_url: row.poster_url,
            air_date: row.air_date,
            air_week: row.air_week,
        }
    }
}
