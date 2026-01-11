//! Trait abstractions for RssProcessingService dependencies.
//!
//! These traits enable testing by allowing mock implementations to replace real dependencies.
//! Each trait represents a specific capability needed by the RSS processing service.

use async_trait::async_trait;
use downloader::DownloaderError;
use parser::ParseResult;
use rss::{FetchContext, FetchResult, RssError, RssSource};

use crate::models::{Bangumi, CreateTorrent, Rss, Settings, Torrent};
use crate::services::washing::{WashParams, WashingError};

// ============================================================================
// Repository Traits
// ============================================================================

/// Trait for accessing Bangumi data.
///
/// Abstracts `BangumiRepository::get_by_id`.
#[async_trait]
pub trait RssBangumiRepository: Send + Sync {
    /// Get a Bangumi by ID.
    async fn get_by_id(&self, id: i64) -> Result<Option<Bangumi>, sqlx::Error>;
}

/// Trait for accessing RSS data.
///
/// Abstracts `RssRepository::update_cache` and `get_by_id`.
#[async_trait]
pub trait RssRssRepository: Send + Sync {
    /// Update RSS cache metadata (ETag, Last-Modified, pubDate).
    async fn update_cache(
        &self,
        id: i64,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) -> Result<(), sqlx::Error>;

    /// Get an RSS subscription by ID.
    async fn get_by_id(&self, id: i64) -> Result<Option<Rss>, sqlx::Error>;
}

/// Trait for accessing Torrent data.
///
/// Abstracts `TorrentRepository` methods used by RSS processing.
#[async_trait]
pub trait RssTorrentRepository: Send + Sync {
    /// Get all torrents for a bangumi.
    async fn get_by_bangumi_id(&self, bangumi_id: i64) -> Result<Vec<Torrent>, sqlx::Error>;

    /// Create a torrent record.
    async fn create(&self, data: CreateTorrent) -> Result<i64, sqlx::Error>;
}

// ============================================================================
// External Service Traits
// ============================================================================

/// Trait for fetching RSS feeds.
///
/// Abstracts `RssClient::fetch_conditional`.
#[async_trait]
pub trait RssFetcher: Send + Sync {
    /// Fetch RSS feed with conditional request support.
    async fn fetch_conditional(
        &self,
        source: &RssSource,
        context: Option<&FetchContext>,
    ) -> Result<FetchResult, RssError>;
}

/// Trait for managing download tasks.
///
/// Abstracts `DownloaderHandle::add_task` and `delete_task`.
#[async_trait]
pub trait RssDownloader: Send + Sync {
    /// Add a download task.
    async fn add_task(
        &self,
        torrent_url: &str,
        save_path: &str,
        rename: &str,
    ) -> Result<String, DownloaderError>;

    /// Delete download tasks.
    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<(), DownloaderError>;
}

/// Trait for torrent washing (洗版) operations.
///
/// Abstracts `WashingService::should_wash` and `wash_episode`.
#[async_trait]
pub trait RssWashingService: Send + Sync {
    /// Determine if we should replace existing torrents with a new one based on priority.
    fn should_wash(&self, existing_torrents: &[Torrent], new_parse_result: &ParseResult) -> bool;

    /// Execute washing: delete old torrents, create new one, and manage downloads.
    async fn wash_episode(&self, params: WashParams<'_>) -> Result<Vec<String>, WashingError>;
}

/// Trait for accessing settings.
///
/// Abstracts `SettingsService::get`.
pub trait RssSettingsProvider: Send + Sync {
    /// Get current settings.
    fn get(&self) -> Settings;
}
