use bgmtv::BgmtvClient;
use mikan::MikanClient;
use reqwest::Client;
use rss::RssClient;
use sqlx::SqlitePool;
use std::sync::Arc;
use tmdb::TmdbClient;

use crate::config::Config;
use crate::services::{
    DownloaderService, FileRenameJob, PosterService, RssFetchJob, SchedulerService,
    SettingsService,
};

#[derive(Clone)]
pub struct AppState {
    pub db: SqlitePool,
    pub config: Arc<Config>,
    pub http_client: Client,
    pub tmdb: Arc<TmdbClient>,
    pub bgmtv: Arc<BgmtvClient>,
    pub mikan: Arc<MikanClient>,
    pub rss: Arc<RssClient>,
    pub settings: Arc<SettingsService>,
    pub downloader: Arc<DownloaderService>,
    pub poster: Arc<PosterService>,
    pub scheduler: Arc<SchedulerService>,
    pub rss_fetch_job: Arc<RssFetchJob>,
}

impl AppState {
    pub fn new(db: SqlitePool, config: Config, settings: SettingsService) -> Self {
        let http_client = Client::new();
        let tmdb = TmdbClient::with_client(http_client.clone(), &config.tmdb_api_key);
        let bgmtv = BgmtvClient::with_client(http_client.clone());
        let mikan = MikanClient::new(http_client.clone());
        let rss = RssClient::with_client(http_client.clone());

        // Wrap settings in Arc first (needed by DownloaderService)
        let settings = Arc::new(settings);

        // Create downloader service with settings reference
        let downloader = DownloaderService::new(Arc::clone(&settings));

        // Create poster service
        let poster = PosterService::new(http_client.clone(), config.posters_path());

        // Create shared Arc references for scheduler jobs
        let rss_arc = Arc::new(rss);
        let downloader_arc = Arc::new(downloader);

        // Create RSS fetch job (stored separately for manual triggering)
        let rss_fetch_job = Arc::new(RssFetchJob::new(
            db.clone(),
            Arc::clone(&rss_arc),
            Arc::clone(&downloader_arc),
        ));

        // Create and start scheduler service
        let scheduler = SchedulerService::new()
            .with_arc_job(Arc::clone(&rss_fetch_job))
            .with_job(FileRenameJob::new());
        scheduler.start();

        Self {
            db,
            config: Arc::new(config),
            http_client,
            tmdb: Arc::new(tmdb),
            bgmtv: Arc::new(bgmtv),
            mikan: Arc::new(mikan),
            rss: rss_arc,
            settings,
            downloader: downloader_arc,
            poster: Arc::new(poster),
            scheduler: Arc::new(scheduler),
            rss_fetch_job,
        }
    }
}
