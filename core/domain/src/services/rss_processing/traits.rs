//! Trait abstractions for RSS processing.
//!
//! This module defines traits that allow mocking database, RSS fetching,
//! and download scheduling for testing purposes.

use async_trait::async_trait;
use downloader::TaskFile;
use rss::{FetchContext, FetchResult, RssSource};

use crate::models::{BangumiWithSeries, CreateTorrent, Torrent};

/// Error type for RSS processing operations
#[derive(Debug, thiserror::Error)]
pub enum RssProcessingError {
    #[error("Database error: {0}")]
    Database(String),
    #[error("Fetch error: {0}")]
    Fetch(String),
    #[error("Downloader error: {0}")]
    Downloader(String),
}

impl From<sqlx::Error> for RssProcessingError {
    fn from(e: sqlx::Error) -> Self {
        RssProcessingError::Database(e.to_string())
    }
}

/// Action to take for a single RSS item
#[derive(Debug, Clone, PartialEq)]
pub enum ProcessAction {
    /// Skip - torrent already exists or lower priority
    Skip { reason: String },
    /// Add new torrent
    Add {
        info_hash: String,
        torrent_url: String,
        episode: i32,
        adjusted_episode: i32,
        filename: String,
        save_path: String,
        bangumi_id: i64,
        rss_id: Option<i64>,
    },
    /// Wash - replace existing torrents with higher priority
    Wash {
        info_hash: String,
        torrent_url: String,
        episode: i32,
        adjusted_episode: i32,
        filename: String,
        save_path: String,
        bangumi_id: i64,
        rss_id: Option<i64>,
        existing_torrent_ids: Vec<i64>,
        existing_info_hashes: Vec<String>,
    },
}

/// Trait for database access operations
#[async_trait]
pub trait RssDataAccess: Send + Sync {
    /// Get bangumi with series info by ID
    async fn get_bangumi_with_series(
        &self,
        bangumi_id: i64,
    ) -> Result<Option<BangumiWithSeries>, RssProcessingError>;

    /// Get all torrents for a bangumi
    async fn get_bangumi_torrents(
        &self,
        bangumi_id: i64,
    ) -> Result<Vec<Torrent>, RssProcessingError>;

    /// Create a new torrent record
    async fn create_torrent(&self, create: CreateTorrent)
        -> Result<Torrent, RssProcessingError>;

    /// Delete torrents by IDs
    async fn delete_torrents(&self, ids: &[i64]) -> Result<(), RssProcessingError>;

    /// Update RSS cache info (etag, last_modified, last_pub_date)
    async fn update_rss_cache(
        &self,
        rss_id: i64,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) -> Result<(), RssProcessingError>;
}

/// Trait for RSS fetching operations
#[async_trait]
pub trait RssFetcher: Send + Sync {
    /// Fetch RSS feed with conditional request support
    async fn fetch_conditional(
        &self,
        source: &RssSource,
        context: Option<&FetchContext>,
    ) -> Result<FetchResult, RssProcessingError>;
}

/// Trait for download task scheduling
#[async_trait]
pub trait TaskScheduler: Send + Sync {
    /// Add a download task
    async fn add_download_task(
        &self,
        options: crate::services::AddTaskOptions,
    ) -> Result<String, RssProcessingError>;

    /// Delete download tasks by info hashes
    async fn delete_tasks(
        &self,
        info_hashes: &[&str],
        delete_files: bool,
    ) -> Result<(), RssProcessingError>;

    /// Get files for a torrent task
    async fn get_task_files(
        &self,
        info_hash: &str,
    ) -> Result<Vec<TaskFile>, RssProcessingError>;
}
