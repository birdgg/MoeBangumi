use std::collections::HashMap;
use std::sync::Arc;

use chrono::Datelike;
use futures::stream::{self, StreamExt};
use metadata::{MetadataSource, SearchedMetadata};

use crate::metadata_service::MetadataService;
use mikan::{MikanClient, Season};
use sqlx::SqlitePool;
use thiserror::Error;

use crate::metadata_service::MetadataHandle;
use crate::models::{CalendarDay, CalendarSubject, CreateMetadata, Platform, SeasonData, Weekday};
use crate::repositories::{CalendarEntry, CalendarRepository, MetadataRepository};
use crate::SeasonIterator;

/// Base URL for downloading calendar seed data from jsDelivr CDN
const CALENDAR_SEED_BASE_URL: &str =
    "https://cdn.jsdelivr.net/gh/birdgg/moe-bangumi@main/assets/seed";

/// Number of seasons to import on first startup (12 seasons = 3 years)
const SEED_SEASONS_COUNT: u32 = 12;

/// Mikan data passed through the calendar refresh pipeline
struct MikanData {
    mikan_id: String,
    air_week: i32,
    poster_url: Option<String>,
}

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
/// Data flow: Mikan Season API → BGM.tv Subject API → Metadata + Calendar tables
pub struct CalendarService {
    db: SqlitePool,
    metadata_service: Arc<MetadataService>,
    mikan: Arc<MikanClient>,
    metadata_actor: Arc<MetadataHandle>,
}

impl CalendarService {
    pub fn new(
        db: SqlitePool,
        metadata_service: Arc<MetadataService>,
        mikan: Arc<MikanClient>,
        metadata_actor: Arc<MetadataHandle>,
    ) -> Self {
        Self {
            db,
            metadata_service,
            mikan,
            metadata_actor,
        }
    }

    /// Get current year and season
    pub fn current_season() -> (i32, Season) {
        let now = chrono::Utc::now();
        let year = now.year();
        let season = Season::current();
        (year, season)
    }

    /// Refresh calendar data for a specific season
    /// 1. Fetch seasonal bangumi list from Mikan
    /// 2. Get BGM.tv ID for each bangumi
    /// 3. Fetch full metadata from BGM.tv
    /// 4. Upsert to metadata and calendar tables
    pub async fn refresh_calendar(&self, year: i32, season: Season) -> Result<usize> {
        let season_str = season.to_db_string();
        tracing::info!("Refreshing calendar for {} {}", year, season_str);

        // Step 1: Get seasonal bangumi list from Mikan
        let seasonal_list = self.mikan.get_seasonal_bangumi_list(year, season).await?;
        tracing::info!(
            "Fetched {} bangumi from Mikan for {} {}",
            seasonal_list.len(),
            year,
            season_str
        );

        if seasonal_list.is_empty() {
            return Ok(0);
        }

        // Step 2: Concurrently fetch BGM.tv IDs (with rate limiting)
        let bgmtv_mappings = self.fetch_bgmtv_ids(&seasonal_list).await;
        tracing::info!(
            "Got {} BGM.tv IDs out of {} bangumi",
            bgmtv_mappings.len(),
            seasonal_list.len()
        );

        if bgmtv_mappings.is_empty() {
            return Ok(0);
        }

        // Step 3: Concurrently fetch BGM.tv subject details
        let subjects = self.fetch_subjects(&bgmtv_mappings).await;
        tracing::info!("Fetched {} subject details from BGM.tv", subjects.len());

        if subjects.is_empty() {
            return Ok(0);
        }

        // Step 4: Ensure metadata exists and get calendar entries
        let calendar_entries = self
            .ensure_metadata_and_build_entries(&subjects, year, &season_str)
            .await?;
        tracing::info!("Built {} calendar entries", calendar_entries.len());

        // Step 5: Batch upsert to calendar table
        CalendarRepository::upsert_batch(&self.db, &calendar_entries).await?;

        // Step 6: Clean up stale entries
        let keep_metadata_ids: Vec<i64> = calendar_entries.iter().map(|e| e.metadata_id).collect();
        let deleted =
            CalendarRepository::delete_except(&self.db, year, &season_str, &keep_metadata_ids)
                .await?;
        if deleted > 0 {
            tracing::info!("Removed {} stale calendar entries", deleted);
        }

        tracing::info!(
            "Successfully refreshed {} calendar entries for {} {}",
            calendar_entries.len(),
            year,
            season_str
        );

        Ok(calendar_entries.len())
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

        let count = self.import_season(year, &season_str).await?;
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
    /// Downloads individual season files for incremental updates
    /// Returns the number of entries imported
    pub async fn import_seed_data(&self) -> Result<usize> {
        let is_empty = CalendarRepository::is_empty(&self.db).await?;

        let count = if is_empty {
            // Full import: download all seasons
            tracing::info!("Calendar is empty, performing full seed import...");
            self.import_all_seasons().await?
        } else {
            // Incremental update: only current and previous season
            tracing::info!("Calendar has data, performing incremental update...");
            self.import_recent_seasons().await?
        };

        // Trigger metadata sync after import (downloads posters and fetches TMDB IDs)
        if count > 0 {
            tracing::info!("Triggering metadata sync after seed import...");
            self.metadata_actor.trigger_sync();
        }

        Ok(count)
    }

    /// Import all seasons from seed data (full import)
    /// Only imports the most recent SEED_SEASONS_COUNT seasons (default: 12 = 3 years)
    async fn import_all_seasons(&self) -> Result<usize> {
        let (current_year, current_season) = Self::current_season();
        let (end_year, end_season) =
            Self::season_n_ago(current_year, current_season, SEED_SEASONS_COUNT);
        let iterator = SeasonIterator::new(current_year, current_season, end_year, end_season);
        let seasons: Vec<_> = iterator.collect();
        let total = seasons.len();

        tracing::info!("Downloading {} seasons from GitHub...", total);

        let mut total_imported = 0;

        for (idx, (year, season)) in seasons.into_iter().enumerate() {
            let season_str = season.to_db_string();

            match self.import_season(year, &season_str).await {
                Ok(count) => {
                    total_imported += count;
                }
                Err(e) => {
                    tracing::warn!(
                        "[{}/{}] Failed to import {} {}: {}",
                        idx + 1,
                        total,
                        year,
                        season_str,
                        e
                    );
                }
            }
        }

        Ok(total_imported)
    }

    /// Import only current and previous seasons (incremental update)
    async fn import_recent_seasons(&self) -> Result<usize> {
        let (current_year, current_season) = Self::current_season();
        let current_season_str = current_season.to_db_string();

        let (prev_year, prev_season) = Self::prev_season(current_year, current_season);
        let prev_season_str = prev_season.to_db_string();

        let mut total_imported = 0;

        // Import current season
        match self.import_season(current_year, &current_season_str).await {
            Ok(count) => {
                total_imported += count;
            }
            Err(e) => {
                tracing::warn!(
                    "Failed to import {} {}: {}",
                    current_year,
                    current_season_str,
                    e
                );
            }
        }

        // Import previous season
        match self.import_season(prev_year, &prev_season_str).await {
            Ok(count) => {
                total_imported += count;
            }
            Err(e) => {
                tracing::warn!("Failed to import {} {}: {}", prev_year, prev_season_str, e);
            }
        }

        tracing::info!(
            "Incremental update complete: {} total entries",
            total_imported
        );
        Ok(total_imported)
    }

    /// Import a single season from GitHub
    async fn import_season(&self, year: i32, season_str: &str) -> Result<usize> {
        let url = format!("{}/{}-{}.json", CALENDAR_SEED_BASE_URL, year, season_str);

        let response = reqwest::get(&url).await?;
        if !response.status().is_success() {
            tracing::debug!("Season file not found: {}", url);
            return Ok(0);
        }

        let json_text = response.text().await?;
        let season_data: SeasonData = serde_json::from_str(&json_text)?;

        let mut calendar_entries = Vec::new();

        for entry in season_data.entries {
            // Check if metadata already exists by bgmtv_id
            let bgmtv_id = match entry.bgmtv_id {
                Some(id) => id,
                None => continue, // Skip entries without bgmtv_id
            };
            let existing = MetadataRepository::get_by_bgmtv_id(&self.db, bgmtv_id).await?;

            let metadata_id = if let Some(metadata) = existing {
                metadata.id
            } else {
                // Create new metadata from seed entry
                let metadata = MetadataRepository::create(&self.db, entry).await?;
                metadata.id
            };

            calendar_entries.push(CalendarEntry {
                metadata_id,
                year: season_data.year,
                season: season_data.season.clone(),
                priority: 0,
            });
        }

        if !calendar_entries.is_empty() {
            CalendarRepository::upsert_batch(&self.db, &calendar_entries).await?;

            // Clean up stale entries for this season
            let keep_metadata_ids: Vec<i64> =
                calendar_entries.iter().map(|e| e.metadata_id).collect();
            let deleted =
                CalendarRepository::delete_except(&self.db, year, season_str, &keep_metadata_ids)
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

    /// Get previous season
    fn prev_season(year: i32, season: Season) -> (i32, Season) {
        match season {
            Season::Winter => (year - 1, Season::Fall),
            Season::Spring => (year, Season::Winter),
            Season::Summer => (year, Season::Spring),
            Season::Fall => (year, Season::Summer),
        }
    }

    /// Get the season N seasons ago
    fn season_n_ago(year: i32, season: Season, n: u32) -> (i32, Season) {
        let mut result = (year, season);
        for _ in 0..n {
            result = Self::prev_season(result.0, result.1);
        }
        result
    }

    /// Concurrently fetch BGM.tv IDs for bangumi list
    /// Returns (MikanData, bgmtv_id) tuples, skipping failures
    async fn fetch_bgmtv_ids(
        &self,
        bangumi_list: &[mikan::SeasonalBangumi],
    ) -> Vec<(MikanData, i64)> {
        // Clone data upfront to avoid lifetime issues
        let items: Vec<_> = bangumi_list
            .iter()
            .map(|b| {
                let data = MikanData {
                    mikan_id: b.mikan_id.clone(),
                    air_week: b.air_week,
                    poster_url: b.poster_url.clone(),
                };
                (data, Arc::clone(&self.mikan))
            })
            .collect();

        let tasks = items.into_iter().map(|(data, mikan)| async move {
            match mikan.get_bangumi_bgmtv_id(&data.mikan_id).await {
                Ok(Some(bgmtv_id)) => Some((data, bgmtv_id)),
                Ok(None) => {
                    tracing::debug!("No BGM.tv ID found for mikan_id: {}", data.mikan_id);
                    None
                }
                Err(e) => {
                    tracing::warn!("Failed to fetch BGM.tv ID for {}: {}", data.mikan_id, e);
                    None
                }
            }
        });

        // Limit concurrency to avoid rate limiting
        stream::iter(tasks)
            .buffer_unordered(5)
            .filter_map(|x| async { x })
            .collect()
            .await
    }

    /// Concurrently fetch BGM.tv subject details
    /// Returns (MikanData, bgmtv_id, SearchedMetadata) tuples, skipping failures
    async fn fetch_subjects(
        &self,
        mappings: &[(MikanData, i64)],
    ) -> Vec<(MikanData, i64, SearchedMetadata)> {
        // Clone data upfront to avoid lifetime issues
        let items: Vec<_> = mappings
            .iter()
            .map(|(data, bgmtv_id)| {
                let data = MikanData {
                    mikan_id: data.mikan_id.clone(),
                    air_week: data.air_week,
                    poster_url: data.poster_url.clone(),
                };
                (data, *bgmtv_id, Arc::clone(&self.metadata_service))
            })
            .collect();

        let tasks = items
            .into_iter()
            .map(|(data, bgmtv_id, metadata_service)| async move {
                match metadata_service
                    .get_provider_detail(&bgmtv_id.to_string(), MetadataSource::Bgmtv)
                    .await
                {
                    Ok(Some(subject)) => Some((data, bgmtv_id, subject)),
                    Ok(None) => {
                        tracing::warn!("BGM.tv subject {} not found", bgmtv_id);
                        None
                    }
                    Err(e) => {
                        tracing::warn!("Failed to fetch BGM.tv subject {}: {}", bgmtv_id, e);
                        None
                    }
                }
            });

        // Limit concurrency
        stream::iter(tasks)
            .buffer_unordered(10)
            .filter_map(|x| async { x })
            .collect()
            .await
    }

    /// Ensure metadata exists for each subject and build calendar entries
    async fn ensure_metadata_and_build_entries(
        &self,
        subjects: &[(MikanData, i64, SearchedMetadata)],
        year: i32,
        season_str: &str,
    ) -> Result<Vec<CalendarEntry>> {
        let mut entries = Vec::new();

        for (mikan_data, bgmtv_id, subject) in subjects {
            // Check if metadata already exists
            let existing = MetadataRepository::get_by_bgmtv_id(&self.db, *bgmtv_id).await?;

            let metadata_id = if let Some(metadata) = existing {
                metadata.id
            } else {
                // Create new metadata with BGM.tv poster
                let create_data = self.build_create_metadata(mikan_data, *bgmtv_id, subject);
                let metadata = MetadataRepository::create(&self.db, create_data).await?;
                metadata.id
            };

            // Priority defaults to 0 (SearchedMetadata doesn't have collection info)
            let priority = 0;

            entries.push(CalendarEntry {
                metadata_id,
                year,
                season: season_str.to_string(),
                priority,
            });
        }

        Ok(entries)
    }

    /// Build CreateMetadata from Mikan data and BGM.tv SearchedMetadata
    fn build_create_metadata(
        &self,
        mikan_data: &MikanData,
        bgmtv_id: i64,
        subject: &SearchedMetadata,
    ) -> CreateMetadata {
        // Use year from metadata or fallback to current year
        let year = subject.year.unwrap_or_else(|| chrono::Utc::now().year());

        // Convert platform from metadata crate to server model
        let platform = subject
            .platform
            .map(|p| match p {
                metadata::Platform::Tv => Platform::Tv,
                metadata::Platform::Movie => Platform::Movie,
                metadata::Platform::Ova => Platform::Ova,
            })
            .unwrap_or(Platform::Tv);

        CreateMetadata {
            mikan_id: Some(mikan_data.mikan_id.clone()),
            bgmtv_id: Some(bgmtv_id),
            tmdb_id: None,
            title_chinese: subject
                .title_chinese
                .clone()
                .or_else(|| subject.title_original.clone())
                .unwrap_or_default(),
            title_japanese: subject.title_original.clone(),
            season: subject.season.unwrap_or(1),
            year,
            platform,
            total_episodes: subject.total_episodes,
            poster_url: subject.poster_url.clone(),
            air_date: subject.air_date.clone(),
            air_week: mikan_data.air_week,
            episode_offset: 0,
        }
    }
}
