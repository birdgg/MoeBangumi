//! Scan Service for importing existing media directories
//!
//! Scans the downloader save path for Plex/Jellyfin formatted directories
//! and automatically imports them as bangumi subscriptions.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, LazyLock};

use metadata::{MetadataSource, SearchQuery};
use regex::Regex;
use sqlx::SqlitePool;
use thiserror::Error;
use tokio::fs;

/// Regex for parsing Plex/Jellyfin directory names
/// Format: "Title (year)" or "Title (year) {tmdb-id}"
static DIR_NAME_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^(.+?)\s*\((\d{4})\)(?:\s*\{tmdb-(\d+)\})?\s*$")
        .expect("Invalid directory name regex")
});

/// Regex for parsing Season directory names
/// Format: "Season XX" or "Season X"
static SEASON_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^[Ss]eason\s*(\d+)$").expect("Invalid season regex")
});

use crate::models::{CreateBangumi, Platform, SourceType};
use crate::repositories::BangumiRepository;
use crate::services::{BangumiService, SettingsService};
use parser::Parser;

#[derive(Debug, Error)]
pub enum ScanError {
    #[error("Failed to read directory: {0}")]
    DirectoryRead(#[from] std::io::Error),

    #[error("Settings error: {0}")]
    Settings(String),

    #[error("Metadata search failed: {0}")]
    MetadataSearch(String),

    #[error("Bangumi creation failed: {0}")]
    BangumiCreation(String),
}

/// Parsed information from a Plex/Jellyfin directory name
#[derive(Debug, Clone)]
pub struct ParsedDirectory {
    pub title: String,
    pub year: i32,
    pub tmdb_id: Option<i64>,
    pub seasons: Vec<i32>,
    pub path: String,
    /// Maximum episode number found in video files
    pub max_episode: Option<i32>,
}

/// Result of scanning and importing a single directory
#[derive(Debug)]
pub enum ImportResult {
    /// Successfully imported
    Imported { title: String, bgmtv_id: i64 },
    /// Skipped because already exists in database
    Skipped { title: String, reason: String },
    /// Failed to import
    Failed { title: String, reason: String },
}

/// Prepared bangumi data ready for batch insert
struct PreparedBangumi {
    title: String,
    bgmtv_id: i64,
    create_data: CreateBangumi,
}

/// Overall scan result
#[derive(Debug, Default)]
pub struct ScanResult {
    pub total_scanned: usize,
    pub imported: usize,
    pub skipped: usize,
    pub failed: usize,
    pub results: Vec<ImportResult>,
}

/// Service for scanning directories and importing bangumi
pub struct ScanService {
    db: SqlitePool,
    bangumi: Arc<BangumiService>,
    client: Arc<metadata::MetadataClient>,
    settings: Arc<SettingsService>,
    /// Flag to prevent concurrent scans
    scan_in_progress: AtomicBool,
}

impl ScanService {
    /// Create a new ScanService
    pub fn new(
        db: SqlitePool,
        bangumi: Arc<BangumiService>,
        client: Arc<metadata::MetadataClient>,
        settings: Arc<SettingsService>,
    ) -> Self {
        Self {
            db,
            bangumi,
            client,
            settings,
            scan_in_progress: AtomicBool::new(false),
        }
    }

    /// Check if a scan is currently in progress
    pub fn is_scan_in_progress(&self) -> bool {
        self.scan_in_progress.load(Ordering::SeqCst)
    }

    /// Try to start a scan. Returns false if a scan is already in progress.
    pub fn try_start_scan(&self) -> bool {
        self.scan_in_progress
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .is_ok()
    }

    /// Mark scan as completed
    fn finish_scan(&self) {
        self.scan_in_progress.store(false, Ordering::SeqCst);
    }

    /// Main scan and import function
    ///
    /// Scans the downloader save path for Plex/Jellyfin formatted directories
    /// and imports them as bangumi subscriptions.
    ///
    /// Note: This method should be called through the API handler which handles
    /// the concurrency check. Direct calls should use `try_start_scan()` first.
    pub async fn scan_and_import(&self) -> ScanResult {
        let result = self.scan_and_import_inner().await;
        self.finish_scan();
        result
    }

    async fn scan_and_import_inner(&self) -> ScanResult {
        let mut result = ScanResult::default();

        // Get save path from settings (use data/downloads in debug mode)
        #[cfg(debug_assertions)]
        let save_path = "data/downloads".to_string();
        #[cfg(not(debug_assertions))]
        let save_path = self.settings.get().downloader.save_path.clone();

        if save_path.is_empty() {
            tracing::warn!("Scan skipped: save_path is not configured");
            return result;
        }

        tracing::info!("Starting directory scan at: {}", save_path);

        // Scan directories
        let directories = match self.scan_directories(&save_path).await {
            Ok(dirs) => dirs,
            Err(e) => {
                tracing::error!("Failed to scan directories: {}", e);
                return result;
            }
        };

        result.total_scanned = directories.len();
        tracing::info!("Found {} valid directories to process", directories.len());

        // Phase 1: Search and validate all directories, collect prepared data
        let mut prepared_bangumi: Vec<PreparedBangumi> = Vec::new();

        for dir in &directories {
            match self.prepare_directory(dir).await {
                Ok(Some(prepared)) => {
                    tracing::info!(
                        "Prepared: {} (bgmtv_id: {}, max_episode: {:?})",
                        prepared.title,
                        prepared.bgmtv_id,
                        dir.max_episode
                    );
                    prepared_bangumi.push(prepared);
                }
                Ok(None) => {
                    // Skipped - already logged in prepare_directory
                }
                Err(import_result) => {
                    result.results.push(import_result);
                }
            }
        }

        tracing::info!(
            "Phase 1 complete: {} prepared for import",
            prepared_bangumi.len()
        );

        // Phase 2: Batch write all prepared bangumi
        for prepared in prepared_bangumi {
            let title = prepared.title.clone();
            let bgmtv_id = prepared.bgmtv_id;

            match self.bangumi.create(prepared.create_data).await {
                Ok(_) => {
                    result.imported += 1;
                    tracing::info!("Imported: {} (bgmtv_id: {})", title, bgmtv_id);
                    result.results.push(ImportResult::Imported { title, bgmtv_id });
                }
                Err(e) => {
                    result.failed += 1;
                    let reason = format!("Failed to create bangumi: {}", e);
                    tracing::warn!("Failed: {} - {}", title, reason);
                    result.results.push(ImportResult::Failed { title, reason });
                }
            }
        }

        // Count skipped from results
        result.skipped = result
            .results
            .iter()
            .filter(|r| matches!(r, ImportResult::Skipped { .. }))
            .count();
        result.failed = result
            .results
            .iter()
            .filter(|r| matches!(r, ImportResult::Failed { .. }))
            .count();

        tracing::info!(
            "Scan completed: {} scanned, {} imported, {} skipped, {} failed",
            result.total_scanned,
            result.imported,
            result.skipped,
            result.failed
        );

        result
    }

    /// Scan the save path for valid Plex/Jellyfin directories
    async fn scan_directories(&self, base_path: &str) -> Result<Vec<ParsedDirectory>, ScanError> {
        let mut valid_directories = Vec::new();

        let mut entries = fs::read_dir(base_path).await?;

        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();

            // Skip if not a directory
            if !path.is_dir() {
                continue;
            }

            let dir_name = match path.file_name().and_then(|n| n.to_str()) {
                Some(name) => name.to_string(),
                None => continue,
            };

            // Skip hidden directories and temporary directories
            if dir_name.starts_with('.') {
                continue;
            }

            // Try to parse the directory name
            if let Some(mut parsed) = self.parse_directory_name(&dir_name) {
                parsed.path = path.to_string_lossy().to_string();

                // Check for Season subdirectories
                let seasons = self.find_seasons(&path).await;
                if seasons.is_empty() {
                    tracing::debug!("Skipping '{}': no Season directories found", dir_name);
                    continue;
                }

                parsed.seasons = seasons;

                // Find max episode from video files
                parsed.max_episode = self.find_max_episode(&path).await;

                valid_directories.push(parsed);
            }
        }

        Ok(valid_directories)
    }

    /// Parse a Plex/Jellyfin formatted directory name
    ///
    /// Supported formats:
    /// - "Title (2024)"
    /// - "Title (2024) {tmdb-12345}"
    fn parse_directory_name(&self, name: &str) -> Option<ParsedDirectory> {
        let caps = DIR_NAME_PATTERN.captures(name)?;

        let title = caps.get(1)?.as_str().trim().to_string();
        let year: i32 = caps.get(2)?.as_str().parse().ok()?;
        let tmdb_id: Option<i64> = caps.get(3).and_then(|m| m.as_str().parse().ok());

        Some(ParsedDirectory {
            title,
            year,
            tmdb_id,
            seasons: Vec::new(),
            path: String::new(),
            max_episode: None,
        })
    }

    /// Find Season subdirectories and return their paths
    async fn find_season_dirs(&self, dir_path: &std::path::Path) -> Vec<(i32, std::path::PathBuf)> {
        let mut season_dirs = Vec::new();

        let Ok(mut entries) = fs::read_dir(dir_path).await else {
            return season_dirs;
        };

        while let Ok(Some(entry)) = entries.next_entry().await {
            let path = entry.path();

            if !path.is_dir() {
                continue;
            }

            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if let Some(caps) = SEASON_PATTERN.captures(name) {
                    if let Some(season_num) = caps.get(1).and_then(|m| m.as_str().parse().ok()) {
                        season_dirs.push((season_num, path));
                    }
                }
            }
        }

        season_dirs.sort_by_key(|(num, _)| *num);
        season_dirs
    }

    /// Find Season subdirectories
    async fn find_seasons(&self, dir_path: &std::path::Path) -> Vec<i32> {
        let mut seasons = Vec::new();

        let Ok(mut entries) = fs::read_dir(dir_path).await else {
            return seasons;
        };

        while let Ok(Some(entry)) = entries.next_entry().await {
            let path = entry.path();

            if !path.is_dir() {
                continue;
            }

            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if let Some(caps) = SEASON_PATTERN.captures(name) {
                    if let Some(season_num) = caps.get(1).and_then(|m| m.as_str().parse().ok()) {
                        seasons.push(season_num);
                    }
                }
            }
        }

        seasons.sort();
        seasons
    }

    /// Find the maximum episode number from video files in Season directories
    async fn find_max_episode(&self, dir_path: &std::path::Path) -> Option<i32> {
        let season_dirs = self.find_season_dirs(dir_path).await;
        let parser = Parser::new();
        let mut max_episode: Option<i32> = None;

        // Video file extensions
        const VIDEO_EXTENSIONS: &[&str] = &["mkv", "mp4", "avi", "wmv", "flv", "mov", "webm"];

        for (_season_num, season_path) in season_dirs {
            let Ok(mut entries) = fs::read_dir(&season_path).await else {
                continue;
            };

            while let Ok(Some(entry)) = entries.next_entry().await {
                let path = entry.path();

                // Skip directories
                if path.is_dir() {
                    continue;
                }

                // Check if it's a video file
                let is_video = path
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| VIDEO_EXTENSIONS.contains(&ext.to_lowercase().as_str()))
                    .unwrap_or(false);

                if !is_video {
                    continue;
                }

                // Try to parse episode number from filename
                if let Some(filename) = path.file_stem().and_then(|n| n.to_str()) {
                    if let Ok(result) = parser.parse(filename) {
                        if let Some(ep) = result.episode {
                            max_episode = Some(max_episode.map_or(ep, |current| current.max(ep)));
                        }
                    }
                }
            }
        }

        max_episode
    }

    /// Prepare a single directory for import (search and validate)
    /// Returns Ok(Some(prepared)) if ready to import, Ok(None) if skipped, Err(result) if failed
    async fn prepare_directory(
        &self,
        dir: &ParsedDirectory,
    ) -> Result<Option<PreparedBangumi>, ImportResult> {
        let title = dir.title.clone();

        // For each season, try to import
        // We'll use the first season for the main import
        let season = dir.seasons.first().copied().unwrap_or(1);

        // Search BGM.tv with year filter
        let query = SearchQuery::new(&dir.title).with_year(dir.year);

        let search_result = match self.client.find(&query, MetadataSource::Bgmtv).await {
            Ok(Some(result)) => result,
            Ok(None) => {
                return Err(ImportResult::Failed {
                    title,
                    reason: "No matching result found on BGM.tv".to_string(),
                });
            }
            Err(e) => {
                return Err(ImportResult::Failed {
                    title,
                    reason: format!("BGM.tv search failed: {}", e),
                });
            }
        };

        let bgmtv_id: i64 = match search_result.external_id.parse() {
            Ok(id) => id,
            Err(_) => {
                return Err(ImportResult::Failed {
                    title,
                    reason: "Invalid BGM.tv ID".to_string(),
                });
            }
        };

        // Check if already exists in database (by bgmtv_id)
        match BangumiRepository::get_by_bgmtv_id(&self.db, bgmtv_id).await {
            Ok(Some(_)) => {
                tracing::debug!(
                    "Skipped: {} - Already exists (bgmtv_id: {})",
                    title,
                    bgmtv_id
                );
                return Ok(None);
            }
            Ok(None) => {
                // Bangumi doesn't exist - will be created
            }
            Err(e) => {
                return Err(ImportResult::Failed {
                    title,
                    reason: format!("Database error: {}", e),
                });
            }
        }

        // Prepare the bangumi data
        let platform = search_result.platform.unwrap_or(Platform::Tv);

        let create_data = CreateBangumi {
            mikan_id: None,
            bgmtv_id: Some(bgmtv_id),
            tmdb_id: dir.tmdb_id,
            title_chinese: search_result
                .title_chinese
                .unwrap_or_else(|| dir.title.clone()),
            title_japanese: search_result.title_japanese,
            season: search_result.season.unwrap_or(season),
            year: search_result.year.unwrap_or(dir.year),
            platform,
            total_episodes: search_result.total_episodes,
            poster_url: search_result.poster_url,
            air_date: search_result.air_date,
            air_week: 0,
            episode_offset: 0,
            auto_complete: true,
            source_type: SourceType::WebRip,
            // Set current_episode from scanned files, or fall back to total_episodes
            current_episode: dir.max_episode.or(Some(search_result.total_episodes)),
            rss_entries: Vec::new(),
            save_path: String::new(),
        };

        Ok(Some(PreparedBangumi {
            title,
            bgmtv_id,
            create_data,
        }))
    }
}
