//! Calendar seed data generator
//!
//! This script fetches historical calendar data from Mikan and BGM.tv,
//! then saves it as individual JSON files per season for incremental updates.
//!
//! Usage:
//!   cargo run --bin calendar_seed           # Fetch all seasons (2013 to current)
//!   cargo run --bin calendar_seed -- --recent 2   # Fetch only recent 2 seasons

use std::sync::Arc;
use std::time::Duration;

use bgmtv::BgmtvClient;
use futures::stream::{self, StreamExt};
use metadata::parse_subject_detail;
use mikan::{MikanClient, Season};
use server::models::{CreateMetadata, Platform, SeasonData};
use server::SeasonIterator;
use tracing_subscriber::EnvFilter;

const OUTPUT_DIR: &str = "assets/seed";
const REQUEST_DELAY_SECS: u64 = 30;
const END_YEAR: i32 = 2013;

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
    let bgmtv = Arc::new(BgmtvClient::new(http_client));

    // Create output directory if needed
    std::fs::create_dir_all(OUTPUT_DIR)?;

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

        match fetch_season_data(&mikan, &bgmtv, year, season).await {
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
                let filename = format!("{}/{}-{}.json", OUTPUT_DIR, year, season_str);
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
    bgmtv: &Arc<BgmtvClient>,
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
            let poster_url = b.poster_url.clone();
            let name = b.name.clone();
            let mikan = Arc::clone(&mikan_clone);
            (mikan_id, air_week, poster_url, name, mikan)
        })
        .collect();

    let bgmtv_mappings: Vec<_> = stream::iter(items)
        .map(|(mikan_id, air_week, poster_url, name, mikan)| async move {
            match mikan.get_bangumi_bgmtv_id(&mikan_id).await {
                Ok(Some(bgmtv_id)) => Some((mikan_id, bgmtv_id, air_week, poster_url, name)),
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

    // Step 3: Fetch BGM.tv subject details
    let bgmtv_clone = Arc::clone(bgmtv);
    let tasks: Vec<_> = bgmtv_mappings
        .into_iter()
        .map(|(mikan_id, bgmtv_id, air_week, poster_url, name)| {
            let bgmtv = Arc::clone(&bgmtv_clone);
            (mikan_id, bgmtv_id, air_week, poster_url, name, bgmtv)
        })
        .collect();

    let entries: Vec<CreateMetadata> = stream::iter(tasks)
        .map(
            |(mikan_id, bgmtv_id, air_week, _poster_url, _name, bgmtv)| async move {
                match bgmtv.get_subject(bgmtv_id).await {
                    Ok(detail) => {
                        // Parse the subject detail
                        let parsed = parse_subject_detail(&detail);

                        // Use year from parsed subject or fallback
                        let subject_year = parsed.year.unwrap_or(year);

                        // Parse platform using FromStr implementation
                        let platform = parsed.platform.parse().unwrap_or(Platform::Tv);

                        Some(CreateMetadata {
                            mikan_id: Some(mikan_id),
                            bgmtv_id: Some(bgmtv_id),
                            tmdb_id: None,
                            title_chinese: parsed
                                .title_chinese
                                .clone()
                                .or_else(|| parsed.title_japanese.clone())
                                .unwrap_or_default(),
                            title_japanese: parsed.title_japanese,
                            season: parsed.season,
                            year: subject_year,
                            platform,
                            total_episodes: parsed.total_episodes as i32,
                            poster_url: Some(parsed.poster_url),
                            air_date: parsed.air_date,
                            air_week,
                            episode_offset: 0,
                        })
                    }
                    Err(e) => {
                        tracing::debug!("Failed to fetch BGM.tv subject {}: {}", bgmtv_id, e);
                        None
                    }
                }
            },
        )
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
