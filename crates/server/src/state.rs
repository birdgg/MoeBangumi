use bgmtv::BgmtvClient;
use mikan::MikanClient;
use reqwest::Client;
use sqlx::SqlitePool;
use std::sync::Arc;
use tmdb::TmdbClient;

use crate::config::Config;
use crate::services::{
    DownloaderService, FileRenameJob, RssFetchJob, SchedulerService, SettingsService,
};

#[derive(Clone)]
pub struct AppState {
    pub db: SqlitePool,
    pub config: Arc<Config>,
    pub http_client: Client,
    pub tmdb: Arc<TmdbClient>,
    pub bgmtv: Arc<BgmtvClient>,
    pub mikan: Arc<MikanClient>,
    pub settings: Arc<SettingsService>,
    pub downloader: Arc<DownloaderService>,
    pub scheduler: Arc<SchedulerService>,
}

impl AppState {
    pub fn new(db: SqlitePool, config: Config, settings: SettingsService) -> Self {
        let http_client = Client::new();
        let tmdb = TmdbClient::with_client(http_client.clone(), &config.tmdb_api_key);
        let bgmtv = BgmtvClient::with_client(http_client.clone());
        let mikan = MikanClient::new(http_client.clone());

        // Create downloader service with settings subscription
        let downloader = DownloaderService::new(settings.get().downloader, settings.subscribe());

        // Create and start scheduler service
        let scheduler = SchedulerService::new()
            .with_job(RssFetchJob::new())
            .with_job(FileRenameJob::new());
        scheduler.start();

        Self {
            db,
            config: Arc::new(config),
            http_client,
            tmdb: Arc::new(tmdb),
            bgmtv: Arc::new(bgmtv),
            mikan: Arc::new(mikan),
            settings: Arc::new(settings),
            downloader: Arc::new(downloader),
            scheduler: Arc::new(scheduler),
        }
    }
}
