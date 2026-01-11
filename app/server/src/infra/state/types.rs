//! State type definitions for the application.
//!
//! This module contains the core state structures that organize
//! the application's dependencies into logical groups.

use std::sync::Arc;

use sqlx::SqlitePool;
use updater::UpdateServiceHandle;

use metadata::MetadataClient;

use crate::config::Config;
use domain::services::actors::metadata::{MetadataHandle, PosterService};
use domain::services::{
    BangumiService, CacheService, CalendarService, DownloaderHandle, HttpClientService,
    LogService, NotificationService, RenameService, ScanService, SettingsService,
};
use jobs::{LogCleanupHandle, RenameHandle, RssFetchHandle};
use mikan::MikanClient;
use rss::RssClient;

/// Infrastructure layer - core dependencies
#[derive(Clone)]
pub struct AppInfra {
    pub db: SqlitePool,
    pub config: Arc<Config>,
    pub http_client: Arc<HttpClientService>,
    pub settings: Arc<SettingsService>,
}

/// API clients layer - external service clients
#[derive(Clone)]
pub struct AppClients {
    pub mikan: Arc<MikanClient>,
    pub rss: Arc<RssClient>,
}

/// Business services layer - core application services
#[derive(Clone)]
pub struct AppServices {
    pub metadata_client: Arc<MetadataClient>,
    pub bangumi: Arc<BangumiService>,
    pub calendar: Arc<CalendarService>,
    pub cache: Arc<CacheService>,
    pub logs: Arc<LogService>,
    pub downloader: Arc<DownloaderHandle>,
    pub poster: Arc<PosterService>,
    pub notification: Arc<NotificationService>,
    pub rename: Arc<RenameService>,
    pub scan: Arc<ScanService>,
    pub update: UpdateServiceHandle,
}

/// Background actors layer - actor handles
#[derive(Clone)]
pub struct AppActors {
    pub metadata: Arc<MetadataHandle>,
    #[allow(dead_code)]
    pub rss_fetch: RssFetchHandle,
    #[allow(dead_code)]
    pub rename: RenameHandle,
    #[allow(dead_code)]
    pub log_cleanup: LogCleanupHandle,
}
