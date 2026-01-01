use std::collections::HashMap;
use std::sync::Arc;

use bgmtv::BgmtvClient;
use chrono::Datelike;
use futures::stream::{self, StreamExt};
use mikan::{MikanClient, Season};
use sqlx::SqlitePool;
use thiserror::Error;

use crate::models::{
    CalendarDay, CalendarSeedData, CalendarSeedEntry, CalendarSubject, CreateMetadata, Platform,
    Weekday,
};
use crate::repositories::{CalendarEntry, CalendarRepository, MetadataRepository};

/// URL to download calendar seed data from GitHub
const CALENDAR_SEED_URL: &str =
    "https://raw.githubusercontent.com/birdgg/moe-bangumi/main/assets/seed/calendar.json";

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

    #[error("BGM.tv API error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),

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
    bgmtv: Arc<BgmtvClient>,
    mikan: Arc<MikanClient>,
}

impl CalendarService {
    pub fn new(db: SqlitePool, bgmtv: Arc<BgmtvClient>, mikan: Arc<MikanClient>) -> Self {
        Self { db, bgmtv, mikan }
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
        tracing::info!(
            "Refreshing calendar for {} {}",
            year,
            season_str
        );

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
        let calendar_entries = self.ensure_metadata_and_build_entries(&subjects, year, &season_str).await?;
        tracing::info!("Built {} calendar entries", calendar_entries.len());

        // Step 5: Batch upsert to calendar table
        CalendarRepository::upsert_batch(&self.db, &calendar_entries).await?;

        // Step 6: Clean up stale entries
        let keep_metadata_ids: Vec<i64> = calendar_entries.iter().map(|e| e.metadata_id).collect();
        let deleted = CalendarRepository::delete_except(&self.db, year, &season_str, &keep_metadata_ids).await?;
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

    /// Import calendar data from GitHub seed JSON
    /// Returns the number of entries imported
    pub async fn import_seed_data(&self) -> Result<usize> {
        tracing::info!("Downloading calendar seed data from GitHub...");

        let response = reqwest::get(CALENDAR_SEED_URL).await?;
        let json_text = response.text().await?;
        let seed_data: CalendarSeedData = serde_json::from_str(&json_text)?;

        tracing::info!(
            "Downloaded seed data: {} seasons",
            seed_data.seasons.len()
        );

        let mut total_imported = 0;

        for season_data in &seed_data.seasons {
            let mut calendar_entries = Vec::new();

            for entry in &season_data.entries {
                // Check if metadata already exists by bgmtv_id
                let existing = MetadataRepository::get_by_bgmtv_id(&self.db, entry.bgmtv_id).await?;

                let metadata_id = if let Some(metadata) = existing {
                    metadata.id
                } else {
                    // Create new metadata from seed entry
                    let create_data = Self::build_create_metadata_from_seed(entry);
                    let metadata = MetadataRepository::create(&self.db, create_data).await?;
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
                total_imported += calendar_entries.len();
            }
        }

        Ok(total_imported)
    }

    /// Build CreateMetadata from seed entry
    fn build_create_metadata_from_seed(entry: &CalendarSeedEntry) -> CreateMetadata {
        let platform = match entry.platform.as_str() {
            "movie" => Platform::Movie,
            "ova" => Platform::Ova,
            _ => Platform::Tv,
        };

        CreateMetadata {
            mikan_id: Some(entry.mikan_id.clone()),
            bgmtv_id: Some(entry.bgmtv_id),
            tmdb_id: None,
            title_chinese: entry.title_chinese.clone(),
            title_japanese: entry.title_japanese.clone(),
            season: 1,
            year: entry.year,
            platform,
            total_episodes: entry.total_episodes,
            poster_url: entry.poster_url.clone(),
            air_date: entry.air_date.clone(),
            air_week: entry.air_week,
            finished: false,
        }
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
    /// Returns (MikanData, bgmtv_id, SubjectDetail) tuples, skipping failures
    async fn fetch_subjects(
        &self,
        mappings: &[(MikanData, i64)],
    ) -> Vec<(MikanData, i64, bgmtv::SubjectDetail)> {
        // Clone data upfront to avoid lifetime issues
        let items: Vec<_> = mappings
            .iter()
            .map(|(data, bgmtv_id)| {
                let data = MikanData {
                    mikan_id: data.mikan_id.clone(),
                    air_week: data.air_week,
                    poster_url: data.poster_url.clone(),
                };
                (data, *bgmtv_id, Arc::clone(&self.bgmtv))
            })
            .collect();

        let tasks = items.into_iter().map(|(data, bgmtv_id, bgmtv)| async move {
            match bgmtv.get_subject(bgmtv_id).await {
                Ok(subject) => Some((data, bgmtv_id, subject)),
                Err(e) => {
                    tracing::warn!(
                        "Failed to fetch BGM.tv subject {}: {}",
                        bgmtv_id,
                        e
                    );
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
        subjects: &[(MikanData, i64, bgmtv::SubjectDetail)],
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
                // Create new metadata (use air_week and poster_url from Mikan)
                let create_data = self.build_create_metadata(mikan_data, *bgmtv_id, subject);
                let metadata = MetadataRepository::create(&self.db, create_data).await?;
                metadata.id
            };

            // Priority defaults to 0 (SubjectDetail doesn't have collection info)
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

    /// Build CreateMetadata from Mikan data and BGM.tv SubjectDetail
    fn build_create_metadata(
        &self,
        mikan_data: &MikanData,
        bgmtv_id: i64,
        subject: &bgmtv::SubjectDetail,
    ) -> CreateMetadata {
        // Parse year from date
        let year = subject
            .date
            .as_ref()
            .and_then(|date| date.get(0..4))
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(|| chrono::Utc::now().year());

        // Parse platform
        let platform = subject
            .platform
            .as_ref()
            .map(|p| match p.to_lowercase().as_str() {
                "movie" | "劇場版" => Platform::Movie,
                "ova" => Platform::Ova,
                _ => Platform::Tv,
            })
            .unwrap_or(Platform::Tv);

        // Use Mikan poster_url, fallback to BGM.tv
        let poster_url = mikan_data
            .poster_url
            .clone()
            .or_else(|| Some(subject.images.large.clone()));

        CreateMetadata {
            mikan_id: Some(mikan_data.mikan_id.clone()),
            bgmtv_id: Some(bgmtv_id),
            tmdb_id: None,
            title_chinese: if subject.name_cn.is_empty() {
                subject.name.clone()
            } else {
                subject.name_cn.clone()
            },
            title_japanese: Some(subject.name.clone()).filter(|s| !s.is_empty()),
            season: 1, // Default to season 1
            year,
            platform,
            total_episodes: subject.total_episodes as i32,
            poster_url,
            air_date: subject.date.clone(),
            air_week: mikan_data.air_week,
            finished: false,
        }
    }
}
