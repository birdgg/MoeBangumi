//! Calendar seed data generator
//!
//! This script fetches historical calendar data from Mikan and BGM.tv,
//! then saves it as individual JSON files per season for incremental updates.
//!
//! Usage:
//!   cargo run -p calendar-seed           # Fetch all seasons (2013 to current)
//!   cargo run -p calendar-seed -- --recent 2   # Fetch only recent 2 seasons

use std::sync::Arc;
use std::time::Duration;

use futures::stream::{self, StreamExt};
use metadata::{BgmtvProvider, MetadataProvider, Platform};
use mikan::{MikanClient, Season, SeasonIterator};
use serde::{Deserialize, Serialize};
use tracing_subscriber::EnvFilter;

fn get_output_dir() -> String {
    std::env::var("OUTPUT_DIR").unwrap_or_else(|_| "assets/seed".to_string())
}
const REQUEST_DELAY_SECS: u64 = 10;
const END_YEAR: i32 = 2013;

/// Seed entry for a single anime
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeedEntry {
    pub mikan_id: Option<String>,
    pub bgmtv_id: Option<i64>,
    pub tmdb_id: Option<i64>,
    pub title_chinese: String,
    pub title_japanese: Option<String>,
    pub season: i32,
    pub year: i32,
    pub platform: Platform,
    pub total_episodes: i32,
    pub poster_url: Option<String>,
    pub air_date: Option<String>,
    pub air_week: i32,
    pub episode_offset: i32,
}

/// Data for a single season
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeasonData {
    pub year: i32,
    pub season: String,
    pub entries: Vec<SeedEntry>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::new("info"))
        .init();

    // Parse command line arguments
    let args: Vec<String> = std::env::args().collect();
    let recent_count = parse_recent_arg(&args);

    if let Some(count) = recent_count {
        tracing::info!(
            "Starting calendar seed data generation (recent {} seasons)",
            count
        );
    } else {
        tracing::info!("Starting calendar seed data generation (all seasons)");
        tracing::info!("Target: {} Winter to current season", END_YEAR);
    }
    tracing::info!(
        "Request delay: {} seconds between seasons",
        REQUEST_DELAY_SECS
    );

    // Create HTTP client
    let http_client = reqwest::Client::builder()
        .user_agent("birdgg/moe-bangumi")
        .timeout(Duration::from_secs(30))
        .build()?;

    // Create API clients
    let mikan = Arc::new(MikanClient::new(http_client.clone()));
    let bgmtv_provider = Arc::new(BgmtvProvider::with_http_client(http_client));

    let output_dir = get_output_dir();

    // Create output directory if needed
    std::fs::create_dir_all(&output_dir)?;

    let iterator = SeasonIterator::from_current_to(END_YEAR, Season::Winter);
    let seasons: Vec<_> = iterator.collect();

    // Limit to recent seasons if specified
    let seasons: Vec<_> = if let Some(count) = recent_count {
        seasons.into_iter().take(count).collect()
    } else {
        seasons
    };
    let total = seasons.len();

    tracing::info!("Will process {} seasons", total);

    let mut total_entries = 0;
    let mut seasons_saved = 0;

    for (idx, (year, season)) in seasons.into_iter().enumerate() {
        let season_str = season.to_db_string();
        tracing::info!(
            "[{}/{}] Processing {} {}...",
            idx + 1,
            total,
            year,
            season_str
        );

        match fetch_season_data(&mikan, &bgmtv_provider, year, season).await {
            Ok(data) => {
                let entry_count = data.entries.len();
                tracing::info!(
                    "[{}/{}] Got {} entries for {} {}",
                    idx + 1,
                    total,
                    entry_count,
                    year,
                    season_str
                );

                // Save each season to its own file
                let filename = format!("{}/{}-{}.json", output_dir, year, season_str);
                let json = serde_json::to_string_pretty(&data)?;
                std::fs::write(&filename, &json)?;
                tracing::info!("Saved {}", filename);

                total_entries += entry_count;
                seasons_saved += 1;
            }
            Err(e) => {
                tracing::warn!(
                    "[{}/{}] Failed to fetch {} {}: {}",
                    idx + 1,
                    total,
                    year,
                    season_str,
                    e
                );
            }
        }

        // Delay between seasons (skip delay after last one)
        if idx + 1 < total {
            tracing::info!(
                "Waiting {} seconds before next request...",
                REQUEST_DELAY_SECS
            );
            tokio::time::sleep(Duration::from_secs(REQUEST_DELAY_SECS)).await;
        }
    }

    tracing::info!(
        "Seed data generation complete: {} seasons, {} total entries",
        seasons_saved,
        total_entries
    );

    Ok(())
}

/// Fetch calendar data for a single season
async fn fetch_season_data(
    mikan: &Arc<MikanClient>,
    bgmtv_provider: &Arc<BgmtvProvider>,
    year: i32,
    season: Season,
) -> Result<SeasonData, Box<dyn std::error::Error>> {
    let season_str = season.to_db_string().to_string();

    // Step 1: Get seasonal bangumi list from Mikan
    let seasonal_list = mikan.get_seasonal_bangumi_list(year, season).await?;
    tracing::debug!("Fetched {} bangumi from Mikan", seasonal_list.len());

    if seasonal_list.is_empty() {
        return Ok(SeasonData {
            year,
            season: season_str,
            entries: Vec::new(),
        });
    }

    // Step 2: Fetch BGM.tv IDs (with concurrency limit)
    let mikan_clone = Arc::clone(mikan);
    let items: Vec<_> = seasonal_list
        .iter()
        .map(|b| {
            let mikan_id = b.mikan_id.clone();
            let air_week = b.air_week;
            let name = b.name.clone();
            let mikan = Arc::clone(&mikan_clone);
            (mikan_id, air_week, name, mikan)
        })
        .collect();

    let bgmtv_mappings: Vec<_> = stream::iter(items)
        .map(|(mikan_id, air_week, name, mikan)| async move {
            match mikan.get_bangumi_bgmtv_id(&mikan_id).await {
                Ok(Some(bgmtv_id)) => Some((mikan_id, bgmtv_id, air_week, name)),
                Ok(None) => {
                    tracing::debug!("No BGM.tv ID for mikan_id: {}", mikan_id);
                    None
                }
                Err(e) => {
                    tracing::debug!("Failed to get BGM.tv ID for {}: {}", mikan_id, e);
                    None
                }
            }
        })
        .buffer_unordered(5)
        .filter_map(|x| async { x })
        .collect()
        .await;

    tracing::debug!(
        "Got {} BGM.tv IDs out of {} bangumi",
        bgmtv_mappings.len(),
        seasonal_list.len()
    );

    if bgmtv_mappings.is_empty() {
        return Ok(SeasonData {
            year,
            season: season_str,
            entries: Vec::new(),
        });
    }

    // Step 3: Fetch BGM.tv subject details via metadata provider
    let provider_clone = Arc::clone(bgmtv_provider);
    let tasks: Vec<_> = bgmtv_mappings
        .into_iter()
        .map(|(mikan_id, bgmtv_id, air_week, _name)| {
            let provider = Arc::clone(&provider_clone);
            (mikan_id, bgmtv_id, air_week, provider)
        })
        .collect();

    let entries: Vec<SeedEntry> = stream::iter(tasks)
        .map(|(mikan_id, bgmtv_id, air_week, provider)| async move {
            let external_id = bgmtv_id.to_string();
            match provider.get_detail(&external_id).await {
                Ok(Some(metadata)) => {
                    let subject_year = metadata.year.unwrap_or(year);
                    let platform = metadata.platform.unwrap_or(Platform::Tv);

                    Some(SeedEntry {
                        mikan_id: Some(mikan_id),
                        bgmtv_id: Some(bgmtv_id),
                        tmdb_id: None,
                        title_chinese: metadata
                            .title_chinese
                            .clone()
                            .or_else(|| metadata.title_original.clone())
                            .unwrap_or_default(),
                        title_japanese: metadata.title_original,
                        season: metadata.season.unwrap_or(1),
                        year: subject_year,
                        platform,
                        total_episodes: metadata.total_episodes,
                        poster_url: metadata.poster_url,
                        air_date: metadata.air_date,
                        air_week,
                        episode_offset: 0,
                    })
                }
                Ok(None) => {
                    tracing::debug!("No detail found for BGM.tv subject {}", bgmtv_id);
                    None
                }
                Err(e) => {
                    tracing::debug!("Failed to fetch BGM.tv subject {}: {}", bgmtv_id, e);
                    None
                }
            }
        })
        .buffer_unordered(10)
        .filter_map(|x| async { x })
        .collect()
        .await;

    Ok(SeasonData {
        year,
        season: season_str,
        entries,
    })
}

/// Parse --recent N argument from command line
fn parse_recent_arg(args: &[String]) -> Option<usize> {
    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        if arg == "--recent" {
            if let Some(count_str) = iter.next() {
                return count_str.parse().ok();
            }
        }
    }
    None
}
