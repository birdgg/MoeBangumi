use std::collections::HashMap;
use std::sync::Arc;

use chrono::Datelike;

use crate::models::{CalendarDay, CalendarSubject, SeasonData, SeedEntry, Weekday};
use crate::repositories::{
    BangumiRepository, CalendarEntry, CalendarRepository, CreateBangumiData, SeriesRepository,
};
use crate::services::actors::metadata::MetadataHandle;
use mikan::Season;
use sqlx::SqlitePool;
use thiserror::Error;

/// Base URL for downloading calendar seed data from GitHub
const CALENDAR_SEED_BASE_URL: &str =
    "https://raw.githubusercontent.com/birdgg/bangumi-calendar/main/data";

#[derive(Debug, Error)]
pub enum CalendarError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Metadata provider error: {0}")]
    Provider(#[from] metadata::ProviderError),

    #[error("Mikan API error: {0}")]
    Mikan(#[from] mikan::MikanError),

    #[error("JSON parse error: {0}")]
    JsonParse(#[from] serde_json::Error),

    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),
}

pub type Result<T> = std::result::Result<T, CalendarError>;

/// Calendar service for managing seasonal anime schedule
/// Data flow: GitHub seed → Series + Bangumi + Calendar tables
pub struct CalendarService {
    db: SqlitePool,
    metadata_actor: Arc<MetadataHandle>,
}

impl CalendarService {
    pub fn new(db: SqlitePool, metadata_actor: Arc<MetadataHandle>) -> Self {
        Self { db, metadata_actor }
    }

    /// Get current year and season
    pub fn current_season() -> (i32, Season) {
        let now = chrono::Utc::now();
        let year = now.year();
        let season = Season::current();
        (year, season)
    }

    /// Get calendar data for a specific season
    pub async fn get_calendar(&self, year: i32, season: Season) -> Result<Vec<CalendarDay>> {
        let season_str = season.to_db_string();
        let entries = CalendarRepository::get_by_season(&self.db, year, &season_str).await?;

        // Group by weekday (1-7)
        let mut weekday_map: HashMap<i32, Vec<CalendarSubject>> = HashMap::new();

        for entry in entries {
            let subject = entry.to_calendar_subject();
            let weekday = entry.air_weekday();
            weekday_map.entry(weekday).or_default().push(subject);
        }

        // Convert to CalendarDay format
        let calendar_days: Vec<CalendarDay> = (1..=7)
            .filter_map(|weekday_id| {
                weekday_map.remove(&weekday_id).map(|items| CalendarDay {
                    weekday: Weekday::from_id(weekday_id),
                    items,
                })
            })
            .collect();

        Ok(calendar_days)
    }

    /// Check if calendar data exists for a season
    pub async fn has_data(&self, year: i32, season: Season) -> Result<bool> {
        let season_str = season.to_db_string();
        Ok(CalendarRepository::has_data(&self.db, year, &season_str).await?)
    }

    /// Check if seed data import is needed (calendar table is empty)
    pub async fn needs_seed_import(&self) -> Result<bool> {
        Ok(CalendarRepository::is_empty(&self.db).await?)
    }

    /// Import a specific season from GitHub seed file if not already in database
    /// This is used for on-demand loading when user requests a historical season
    pub async fn import_season_if_missing(&self, year: i32, season: Season) -> Result<usize> {
        let season_str = season.to_db_string();

        // Check if data already exists for this season
        if CalendarRepository::has_data(&self.db, year, &season_str).await? {
            tracing::debug!(
                "Calendar data for {} {} already exists, skipping import",
                year,
                season_str
            );
            return Ok(0);
        }

        let count = self.import_season(year, season).await?;
        if count > 0 {
            tracing::info!(
                "Imported {} entries for {} {} on demand",
                count,
                year,
                season_str
            );
            self.metadata_actor.trigger_sync();
        }
        Ok(count)
    }

    /// Import calendar data from GitHub seed JSON files
    /// Downloads current and previous season files
    /// Returns the number of entries imported
    pub async fn import_seed_data(&self) -> Result<usize> {
        tracing::info!("Importing recent seasons from seed data...");

        let (current_year, current_season) = Self::current_season();
        let (prev_year, prev_season) = Self::prev_season(current_year, current_season);

        let mut total_imported = 0;

        // Import current season
        match self.import_season(current_year, current_season).await {
            Ok(count) => total_imported += count,
            Err(e) => tracing::warn!(
                "Failed to import {} {:?}: {}",
                current_year,
                current_season,
                e
            ),
        }

        // Import previous season
        match self.import_season(prev_year, prev_season).await {
            Ok(count) => total_imported += count,
            Err(e) => tracing::warn!("Failed to import {} {:?}: {}", prev_year, prev_season, e),
        }

        // Trigger metadata sync after import (downloads posters and fetches TMDB IDs)
        if total_imported > 0 {
            tracing::info!(
                "Imported {} entries, triggering metadata sync...",
                total_imported
            );
            self.metadata_actor.trigger_sync();
        }

        Ok(total_imported)
    }

    /// Import a single season from GitHub
    pub async fn import_season(&self, year: i32, season: Season) -> Result<usize> {
        let season_str = season.to_db_string();
        let url = format!("{}/{}-{}.json", CALENDAR_SEED_BASE_URL, year, season_str);

        let response = reqwest::get(&url).await?;
        if !response.status().is_success() {
            return Ok(0);
        }

        let json_text = response.text().await?;
        let season_data: SeasonData = serde_json::from_str(&json_text)?;

        // Filter entries with bgmtv_id
        let entries_with_bgmtv: Vec<_> = season_data
            .entries
            .into_iter()
            .filter(|e| e.bgmtv_id.is_some())
            .collect();

        if entries_with_bgmtv.is_empty() {
            return Ok(0);
        }

        // Batch query: get all existing bangumi IDs by bgmtv_id in one query
        let bgmtv_ids: Vec<i64> = entries_with_bgmtv
            .iter()
            .filter_map(|e| e.bgmtv_id)
            .collect();

        let existing_map = self.get_existing_bangumi_by_bgmtv(&bgmtv_ids).await?;

        // Separate entries into existing and new
        let (existing_entries, new_entries): (Vec<_>, Vec<_>) = entries_with_bgmtv
            .into_iter()
            .partition(|e| e.bgmtv_id.map_or(false, |id| existing_map.contains_key(&id)));

        // Create new bangumi entries (with series)
        let created_map = self.batch_create_bangumi(new_entries).await?;

        // Build calendar entries
        let mut calendar_entries = Vec::new();

        // Add entries with existing bangumi
        for entry in existing_entries {
            if let Some(bgmtv_id) = entry.bgmtv_id {
                if let Some(&bangumi_id) = existing_map.get(&bgmtv_id) {
                    calendar_entries.push(CalendarEntry {
                        bangumi_id,
                        year: season_data.year,
                        season: season_data.season.clone(),
                        priority: 0,
                    });
                }
            }
        }

        // Add entries with newly created bangumi
        for (_bgmtv_id, bangumi_id) in created_map {
            calendar_entries.push(CalendarEntry {
                bangumi_id,
                year: season_data.year,
                season: season_data.season.clone(),
                priority: 0,
            });
        }

        if !calendar_entries.is_empty() {
            CalendarRepository::upsert_batch(&self.db, &calendar_entries).await?;

            // Clean up stale entries for this season
            let keep_bangumi_ids: Vec<i64> =
                calendar_entries.iter().map(|e| e.bangumi_id).collect();
            let deleted = CalendarRepository::delete_except(
                &self.db,
                year,
                &season_str,
                &keep_bangumi_ids,
            )
            .await?;
            if deleted > 0 {
                tracing::debug!(
                    "Removed {} stale entries for {} {}",
                    deleted,
                    year,
                    season_str
                );
            }
        }

        Ok(calendar_entries.len())
    }

    /// Get existing bangumi IDs mapped by bgmtv_id
    async fn get_existing_bangumi_by_bgmtv(
        &self,
        bgmtv_ids: &[i64],
    ) -> Result<HashMap<i64, i64>> {
        let mut result = HashMap::new();

        for &bgmtv_id in bgmtv_ids {
            if let Some(bangumi) = BangumiRepository::get_by_bgmtv_id(&self.db, bgmtv_id).await? {
                result.insert(bgmtv_id, bangumi.id);
            }
        }

        Ok(result)
    }

    /// Batch create bangumi from seed entries
    /// Returns map of bgmtv_id -> bangumi_id
    async fn batch_create_bangumi(
        &self,
        entries: Vec<SeedEntry>,
    ) -> Result<HashMap<i64, i64>> {
        let mut result = HashMap::new();

        for entry in entries {
            let Some(bgmtv_id) = entry.bgmtv_id else {
                continue;
            };

            // Find or create series
            let create_series = crate::models::CreateSeries {
                tmdb_id: entry.tmdb_id,
                title_chinese: entry.title_chinese.clone(),
                title_japanese: entry.title_japanese.clone(),
                poster_url: entry.poster_url.clone(),
            };

            let (series, _) = SeriesRepository::find_or_create(&self.db, create_series).await?;

            // Create bangumi
            let create_data = CreateBangumiData {
                series_id: series.id,
                mikan_id: entry.mikan_id,
                bgmtv_id: Some(bgmtv_id),
                title_chinese: entry.title_chinese,
                title_japanese: entry.title_japanese,
                season: entry.season,
                year: entry.year,
                total_episodes: entry.total_episodes,
                poster_url: entry.poster_url,
                air_date: entry.air_date,
                air_week: entry.air_week,
                platform: entry.platform,
                current_episode: 0,
                episode_offset: 0,
                auto_complete: true,
                source_type: "webrip".to_string(),
            };

            let bangumi = BangumiRepository::create(&self.db, create_data).await?;
            result.insert(bgmtv_id, bangumi.id);
        }

        Ok(result)
    }

    /// Get previous season
    fn prev_season(year: i32, season: Season) -> (i32, Season) {
        match season {
            Season::Winter => (year - 1, Season::Fall),
            Season::Spring => (year, Season::Winter),
            Season::Summer => (year, Season::Spring),
            Season::Fall => (year, Season::Summer),
        }
    }
}
