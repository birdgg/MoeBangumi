//! Adapter implementations for production types.
//!
//! These adapters wrap existing production types to implement the trait abstractions,
//! allowing seamless integration without changing existing code.

use async_trait::async_trait;
use downloader::{AddTaskOptions, DownloaderError};
use parser::ParseResult;
use rss::{FetchContext, FetchResult, RssClient, RssError, RssSource};
use sqlx::SqlitePool;
use std::sync::Arc;

use crate::models::{Bangumi, CreateTorrent, Rss, Settings, Torrent};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::washing::{WashParams, WashingError, WashingService};
use crate::services::{DownloaderHandle, SettingsService};

use super::traits::*;

// ============================================================================
// Repository Adapters
// ============================================================================

/// Adapter for BangumiRepository to implement RssBangumiRepository trait.
#[derive(Clone)]
pub struct BangumiRepositoryAdapter {
    pub(crate) db: SqlitePool,
}

impl BangumiRepositoryAdapter {
    pub fn new(db: SqlitePool) -> Self {
        Self { db }
    }
}

#[async_trait]
impl RssBangumiRepository for BangumiRepositoryAdapter {
    async fn get_by_id(&self, id: i64) -> Result<Option<Bangumi>, sqlx::Error> {
        BangumiRepository::get_by_id(&self.db, id).await
    }
}

/// Adapter for RssRepository to implement RssRssRepository trait.
#[derive(Clone)]
pub struct RssRepositoryAdapter {
    pub(crate) db: SqlitePool,
}

impl RssRepositoryAdapter {
    pub fn new(db: SqlitePool) -> Self {
        Self { db }
    }
}

#[async_trait]
impl RssRssRepository for RssRepositoryAdapter {
    async fn update_cache(
        &self,
        id: i64,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) -> Result<(), sqlx::Error> {
        RssRepository::update_cache(&self.db, id, etag, last_modified, last_pub_date).await
    }

    async fn get_by_id(&self, id: i64) -> Result<Option<Rss>, sqlx::Error> {
        RssRepository::get_by_id(&self.db, id).await
    }
}

/// Adapter for TorrentRepository to implement RssTorrentRepository trait.
#[derive(Clone)]
pub struct TorrentRepositoryAdapter {
    pub(crate) db: SqlitePool,
}

impl TorrentRepositoryAdapter {
    pub fn new(db: SqlitePool) -> Self {
        Self { db }
    }
}

#[async_trait]
impl RssTorrentRepository for TorrentRepositoryAdapter {
    async fn get_by_bangumi_id(&self, bangumi_id: i64) -> Result<Vec<Torrent>, sqlx::Error> {
        TorrentRepository::get_by_bangumi_id(&self.db, bangumi_id).await
    }

    async fn create(&self, data: CreateTorrent) -> Result<i64, sqlx::Error> {
        TorrentRepository::create_with_executor(&self.db, data).await
    }
}

// ============================================================================
// External Service Adapters
// ============================================================================

/// Adapter for RssClient to implement RssFetcher trait.
#[derive(Clone)]
pub struct RssClientAdapter {
    pub(crate) client: Arc<RssClient>,
}

impl RssClientAdapter {
    pub fn new(client: Arc<RssClient>) -> Self {
        Self { client }
    }
}

#[async_trait]
impl RssFetcher for RssClientAdapter {
    async fn fetch_conditional(
        &self,
        source: &RssSource,
        context: Option<&FetchContext>,
    ) -> Result<FetchResult, RssError> {
        self.client.fetch_conditional(source, context).await
    }
}

/// Adapter for DownloaderHandle to implement RssDownloader trait.
#[derive(Clone)]
pub struct DownloaderAdapter {
    pub(crate) handle: Arc<DownloaderHandle>,
}

impl DownloaderAdapter {
    pub fn new(handle: Arc<DownloaderHandle>) -> Self {
        Self { handle }
    }
}

#[async_trait]
impl RssDownloader for DownloaderAdapter {
    async fn add_task(
        &self,
        torrent_url: &str,
        save_path: &str,
        rename: &str,
    ) -> Result<String, DownloaderError> {
        let options = AddTaskOptions::new(torrent_url)
            .save_path(save_path)
            .rename(rename);
        self.handle.add_task(options).await
    }

    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<(), DownloaderError> {
        self.handle.delete_task(ids, delete_files).await
    }
}

/// Adapter for WashingService to implement RssWashingService trait.
#[derive(Clone)]
pub struct WashingServiceAdapter {
    pub(crate) service: Arc<WashingService>,
}

impl WashingServiceAdapter {
    pub fn new(service: Arc<WashingService>) -> Self {
        Self { service }
    }
}

#[async_trait]
impl RssWashingService for WashingServiceAdapter {
    fn should_wash(&self, existing_torrents: &[Torrent], new_parse_result: &ParseResult) -> bool {
        self.service.should_wash(existing_torrents, new_parse_result)
    }

    async fn wash_episode(&self, params: WashParams<'_>) -> Result<Vec<String>, WashingError> {
        self.service.wash_episode(params).await
    }
}

/// Adapter for SettingsService to implement RssSettingsProvider trait.
#[derive(Clone)]
pub struct SettingsAdapter {
    pub(crate) service: Arc<SettingsService>,
}

impl SettingsAdapter {
    pub fn new(service: Arc<SettingsService>) -> Self {
        Self { service }
    }
}

impl RssSettingsProvider for SettingsAdapter {
    fn get(&self) -> Settings {
        self.service.get()
    }
}
