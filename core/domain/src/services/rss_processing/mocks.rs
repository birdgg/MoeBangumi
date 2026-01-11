//! Mock implementations for testing RssProcessingService.
//!
//! This module provides ready-to-use mock implementations of all traits
//! required by `GenericRssProcessingService`. Use these in unit tests to
//! control and verify the behavior of the RSS processing service.
//!
//! # Example
//!
//! ```ignore
//! use crate::services::rss_processing::mocks::*;
//! use crate::services::rss_processing::GenericRssProcessingService;
//!
//! #[tokio::test]
//! async fn test_process_single() {
//!     let bangumi_repo = MockBangumiRepository::new();
//!     bangumi_repo.insert(test_bangumi());
//!
//!     let rss_fetcher = MockRssFetcher::new();
//!     rss_fetcher.set_result(FetchResult::NotModified);
//!
//!     let service = GenericRssProcessingService::new(
//!         bangumi_repo,
//!         MockRssRepository::new(),
//!         MockTorrentRepository::new(),
//!         rss_fetcher,
//!         MockDownloader::new(),
//!         MockWashingService::new(false),
//!         MockSettingsProvider::default(),
//!     );
//!
//!     service.process_single(&test_rss, &[]).await;
//!     // Assert expectations...
//! }
//! ```

use async_trait::async_trait;
use downloader::DownloaderError;
use parser::ParseResult;
use rss::{FetchContext, FetchResult, RssError, RssSource};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::models::{Bangumi, CreateTorrent, Rss, Settings, Torrent};
use crate::services::washing::{WashParams, WashingError};

use super::traits::*;

// ============================================================================
// Mock Bangumi Repository
// ============================================================================

/// Mock implementation of RssBangumiRepository for testing.
#[derive(Clone, Default)]
pub struct MockBangumiRepository {
    data: Arc<Mutex<HashMap<i64, Bangumi>>>,
}

impl MockBangumiRepository {
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a bangumi for testing.
    pub fn insert(&self, bangumi: Bangumi) {
        self.data.lock().unwrap().insert(bangumi.id, bangumi);
    }

    /// Get all stored bangumi (for verification).
    pub fn get_all(&self) -> Vec<Bangumi> {
        self.data.lock().unwrap().values().cloned().collect()
    }
}

#[async_trait]
impl RssBangumiRepository for MockBangumiRepository {
    async fn get_by_id(&self, id: i64) -> Result<Option<Bangumi>, sqlx::Error> {
        Ok(self.data.lock().unwrap().get(&id).cloned())
    }
}

// ============================================================================
// Mock RSS Repository
// ============================================================================

/// Mock implementation of RssRssRepository for testing.
#[derive(Clone, Default)]
pub struct MockRssRepository {
    data: Arc<Mutex<HashMap<i64, Rss>>>,
    cache_updates: Arc<Mutex<Vec<CacheUpdate>>>,
}

/// Recorded cache update for verification.
#[derive(Clone, Debug)]
pub struct CacheUpdate {
    pub id: i64,
    pub etag: Option<String>,
    pub last_modified: Option<String>,
    pub last_pub_date: Option<String>,
}

impl MockRssRepository {
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert an RSS subscription for testing.
    pub fn insert(&self, rss: Rss) {
        self.data.lock().unwrap().insert(rss.id, rss);
    }

    /// Get all recorded cache updates (for verification).
    pub fn get_cache_updates(&self) -> Vec<CacheUpdate> {
        self.cache_updates.lock().unwrap().clone()
    }
}

#[async_trait]
impl RssRssRepository for MockRssRepository {
    async fn update_cache(
        &self,
        id: i64,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) -> Result<(), sqlx::Error> {
        self.cache_updates.lock().unwrap().push(CacheUpdate {
            id,
            etag,
            last_modified,
            last_pub_date,
        });
        Ok(())
    }

    async fn get_by_id(&self, id: i64) -> Result<Option<Rss>, sqlx::Error> {
        Ok(self.data.lock().unwrap().get(&id).cloned())
    }
}

// ============================================================================
// Mock Torrent Repository
// ============================================================================

/// Mock implementation of RssTorrentRepository for testing.
#[derive(Clone, Default)]
pub struct MockTorrentRepository {
    torrents_by_bangumi: Arc<Mutex<HashMap<i64, Vec<Torrent>>>>,
    created_torrents: Arc<Mutex<Vec<CreateTorrent>>>,
    next_id: Arc<Mutex<i64>>,
}

impl MockTorrentRepository {
    pub fn new() -> Self {
        Self {
            next_id: Arc::new(Mutex::new(1)),
            ..Default::default()
        }
    }

    /// Set existing torrents for a bangumi.
    pub fn set_torrents(&self, bangumi_id: i64, torrents: Vec<Torrent>) {
        self.torrents_by_bangumi
            .lock()
            .unwrap()
            .insert(bangumi_id, torrents);
    }

    /// Get all created torrents (for verification).
    pub fn get_created_torrents(&self) -> Vec<CreateTorrent> {
        self.created_torrents.lock().unwrap().clone()
    }
}

#[async_trait]
impl RssTorrentRepository for MockTorrentRepository {
    async fn get_by_bangumi_id(&self, bangumi_id: i64) -> Result<Vec<Torrent>, sqlx::Error> {
        Ok(self
            .torrents_by_bangumi
            .lock()
            .unwrap()
            .get(&bangumi_id)
            .cloned()
            .unwrap_or_default())
    }

    async fn create(&self, data: CreateTorrent) -> Result<i64, sqlx::Error> {
        let mut id = self.next_id.lock().unwrap();
        let current_id = *id;
        *id += 1;
        self.created_torrents.lock().unwrap().push(data);
        Ok(current_id)
    }
}

// ============================================================================
// Mock RSS Fetcher
// ============================================================================

/// Mock implementation of RssFetcher for testing.
#[derive(Clone, Default)]
pub struct MockRssFetcher {
    result: Arc<Mutex<Option<FetchResult>>>,
    fetch_count: Arc<Mutex<usize>>,
}

impl MockRssFetcher {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the result to return on fetch.
    pub fn set_result(&self, result: FetchResult) {
        *self.result.lock().unwrap() = Some(result);
    }

    /// Get the number of fetch calls (for verification).
    pub fn get_fetch_count(&self) -> usize {
        *self.fetch_count.lock().unwrap()
    }
}

#[async_trait]
impl RssFetcher for MockRssFetcher {
    async fn fetch_conditional(
        &self,
        _source: &RssSource,
        _context: Option<&FetchContext>,
    ) -> Result<FetchResult, RssError> {
        *self.fetch_count.lock().unwrap() += 1;
        self.result
            .lock()
            .unwrap()
            .clone()
            .ok_or_else(|| RssError::Parse("No mock result set".into()))
    }
}

// ============================================================================
// Mock Downloader
// ============================================================================

/// Mock implementation of RssDownloader for testing.
#[derive(Clone, Default)]
pub struct MockDownloader {
    added_tasks: Arc<Mutex<Vec<AddedTask>>>,
    deleted_tasks: Arc<Mutex<Vec<DeletedTask>>>,
    should_fail: Arc<Mutex<bool>>,
}

/// Recorded added task for verification.
#[derive(Clone, Debug)]
pub struct AddedTask {
    pub torrent_url: String,
    pub save_path: String,
    pub rename: String,
}

/// Recorded deleted task for verification.
#[derive(Clone, Debug)]
pub struct DeletedTask {
    pub ids: Vec<String>,
    pub delete_files: bool,
}

impl MockDownloader {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set whether add_task should fail.
    pub fn set_should_fail(&self, should_fail: bool) {
        *self.should_fail.lock().unwrap() = should_fail;
    }

    /// Get all added tasks (for verification).
    pub fn get_added_tasks(&self) -> Vec<AddedTask> {
        self.added_tasks.lock().unwrap().clone()
    }

    /// Get all deleted tasks (for verification).
    pub fn get_deleted_tasks(&self) -> Vec<DeletedTask> {
        self.deleted_tasks.lock().unwrap().clone()
    }
}

#[async_trait]
impl RssDownloader for MockDownloader {
    async fn add_task(
        &self,
        torrent_url: &str,
        save_path: &str,
        rename: &str,
    ) -> Result<String, DownloaderError> {
        if *self.should_fail.lock().unwrap() {
            return Err(DownloaderError::ServiceUnavailable);
        }
        self.added_tasks.lock().unwrap().push(AddedTask {
            torrent_url: torrent_url.to_string(),
            save_path: save_path.to_string(),
            rename: rename.to_string(),
        });
        Ok("mock_hash".to_string())
    }

    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<(), DownloaderError> {
        self.deleted_tasks.lock().unwrap().push(DeletedTask {
            ids: ids.iter().map(|s| s.to_string()).collect(),
            delete_files,
        });
        Ok(())
    }
}

// ============================================================================
// Mock Washing Service
// ============================================================================

/// Mock implementation of RssWashingService for testing.
#[derive(Clone)]
pub struct MockWashingService {
    should_wash_result: bool,
    wash_calls: Arc<Mutex<Vec<i32>>>,
}

impl MockWashingService {
    /// Create a mock that returns the specified value for should_wash.
    pub fn new(should_wash: bool) -> Self {
        Self {
            should_wash_result: should_wash,
            wash_calls: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Get all episodes that were washed (for verification).
    pub fn get_wash_calls(&self) -> Vec<i32> {
        self.wash_calls.lock().unwrap().clone()
    }
}

impl Default for MockWashingService {
    fn default() -> Self {
        Self::new(false)
    }
}

#[async_trait]
impl RssWashingService for MockWashingService {
    fn should_wash(&self, _existing_torrents: &[Torrent], _new_parse_result: &ParseResult) -> bool {
        self.should_wash_result
    }

    async fn wash_episode(&self, params: WashParams<'_>) -> Result<Vec<String>, WashingError> {
        self.wash_calls.lock().unwrap().push(params.episode);
        Ok(vec![])
    }
}

// ============================================================================
// Mock Settings Provider
// ============================================================================

/// Mock implementation of RssSettingsProvider for testing.
#[derive(Clone)]
pub struct MockSettingsProvider {
    settings: Settings,
}

impl MockSettingsProvider {
    pub fn new(settings: Settings) -> Self {
        Self { settings }
    }
}

impl Default for MockSettingsProvider {
    fn default() -> Self {
        Self::new(Settings::default())
    }
}

impl RssSettingsProvider for MockSettingsProvider {
    fn get(&self) -> Settings {
        self.settings.clone()
    }
}
