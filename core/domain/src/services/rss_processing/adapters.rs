//! Default implementations of RSS processing traits.

use async_trait::async_trait;
use downloader::TaskFile;
use rss::{FetchContext, FetchResult, RssClient, RssSource};
use sqlx::SqlitePool;
use std::sync::Arc;

use crate::models::{BangumiWithSeries, CreateTorrent, Torrent};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::{AddTaskOptions, DownloaderHandle};

use super::traits::{RssDataAccess, RssFetcher, RssProcessingError, TaskScheduler};

/// Default implementation of RssDataAccess using SQLite
pub struct SqliteRssDataAccess {
    db: SqlitePool,
}

impl SqliteRssDataAccess {
    pub fn new(db: SqlitePool) -> Self {
        Self { db }
    }
}

#[async_trait]
impl RssDataAccess for SqliteRssDataAccess {
    async fn get_bangumi_with_series(
        &self,
        bangumi_id: i64,
    ) -> Result<Option<BangumiWithSeries>, RssProcessingError> {
        BangumiRepository::get_with_series_by_id(&self.db, bangumi_id)
            .await
            .map_err(|e| RssProcessingError::Database(e.to_string()))
    }

    async fn get_bangumi_torrents(
        &self,
        bangumi_id: i64,
    ) -> Result<Vec<Torrent>, RssProcessingError> {
        TorrentRepository::get_by_bangumi_id(&self.db, bangumi_id)
            .await
            .map_err(|e| RssProcessingError::Database(e.to_string()))
    }

    async fn create_torrent(
        &self,
        create: CreateTorrent,
    ) -> Result<Torrent, RssProcessingError> {
        TorrentRepository::create(&self.db, create)
            .await
            .map_err(|e| RssProcessingError::Database(e.to_string()))
    }

    async fn delete_torrents(&self, ids: &[i64]) -> Result<(), RssProcessingError> {
        for id in ids {
            TorrentRepository::delete(&self.db, *id)
                .await
                .map_err(|e| RssProcessingError::Database(e.to_string()))?;
        }
        Ok(())
    }

    async fn update_rss_cache(
        &self,
        rss_id: i64,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) -> Result<(), RssProcessingError> {
        RssRepository::update_cache(&self.db, rss_id, etag, last_modified, last_pub_date)
            .await
            .map_err(|e| RssProcessingError::Database(e.to_string()))
    }
}

/// Default implementation of RssFetcher using RssClient
pub struct DefaultRssFetcher {
    client: Arc<RssClient>,
}

impl DefaultRssFetcher {
    pub fn new(client: Arc<RssClient>) -> Self {
        Self { client }
    }
}

#[async_trait]
impl RssFetcher for DefaultRssFetcher {
    async fn fetch_conditional(
        &self,
        source: &RssSource,
        context: Option<&FetchContext>,
    ) -> Result<FetchResult, RssProcessingError> {
        self.client
            .fetch_conditional(source, context)
            .await
            .map_err(|e| RssProcessingError::Fetch(e.to_string()))
    }
}

/// Default implementation of TaskScheduler using DownloaderHandle
pub struct DefaultTaskScheduler {
    downloader: Arc<DownloaderHandle>,
}

impl DefaultTaskScheduler {
    pub fn new(downloader: Arc<DownloaderHandle>) -> Self {
        Self { downloader }
    }
}

#[async_trait]
impl TaskScheduler for DefaultTaskScheduler {
    async fn add_download_task(
        &self,
        options: AddTaskOptions,
    ) -> Result<String, RssProcessingError> {
        self.downloader
            .add_task(options)
            .await
            .map_err(|e| RssProcessingError::Downloader(e.to_string()))
    }

    async fn delete_tasks(
        &self,
        info_hashes: &[&str],
        delete_files: bool,
    ) -> Result<(), RssProcessingError> {
        self.downloader
            .delete_task(info_hashes, delete_files)
            .await
            .map_err(|e| RssProcessingError::Downloader(e.to_string()))
    }

    async fn get_task_files(
        &self,
        info_hash: &str,
    ) -> Result<Vec<TaskFile>, RssProcessingError> {
        self.downloader
            .get_task_files(info_hash)
            .await
            .map_err(|e| RssProcessingError::Downloader(e.to_string()))
    }
}
