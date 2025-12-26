use bgmtv::BgmtvClient;
use mikan::MikanClient;
use reqwest::Client;
use rss::RssClient;
use sqlx::SqlitePool;
use std::sync::Arc;
use tmdb::TmdbClient;

use crate::config::Config;
use crate::services::{
    BangumiService, CacheService, DownloaderService, FileRenameJob, LogCleanupJob, LogService,
    PosterService, RssFetchJob, SchedulerService, SettingsService,
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
    pub logs: Arc<LogService>,
    pub bangumi: Arc<BangumiService>,
    pub cache: Arc<CacheService>,
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

        // Create log service for logging and notifications
        let logs = Arc::new(LogService::new(db.clone()));

        // Create downloader service with settings reference
        let downloader = DownloaderService::new(Arc::clone(&settings));

        // Create poster service
        let poster = Arc::new(PosterService::new(http_client.clone(), config.posters_path()));

        // Create bangumi service
        let bangumi = Arc::new(BangumiService::new(db.clone(), Arc::clone(&poster)));

        // Create cache service
        let cache = Arc::new(CacheService::new(db.clone()));

        // Create shared Arc references for scheduler jobs
        let rss_arc = Arc::new(rss);
        let downloader_arc = Arc::new(downloader);

        // Create RSS fetch job (stored separately for manual triggering)
        let rss_fetch_job = Arc::new(RssFetchJob::new(
            db.clone(),
            Arc::clone(&rss_arc),
            Arc::clone(&downloader_arc),
            Arc::clone(&settings),
        ));

        // Create and start scheduler service
        let scheduler = SchedulerService::new()
            .with_arc_job(Arc::clone(&rss_fetch_job))
            .with_job(FileRenameJob::new(db.clone(), Arc::clone(&downloader_arc)))
            .with_job(LogCleanupJob::new(Arc::clone(&logs)));
        scheduler.start();

        // Configure webhook on startup if webhook_url is set
        let startup_settings = settings.get();
        if !startup_settings.downloader.webhook_url.is_empty() {
            let downloader_for_webhook = Arc::clone(&downloader_arc);
            let webhook_url = startup_settings.downloader.webhook_url.clone();
            tokio::spawn(async move {
                if let Err(e) = downloader_for_webhook.configure_autorun(&webhook_url).await {
                    tracing::warn!("Failed to configure downloader webhook on startup: {}", e);
                }
            });
        }

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
            poster,
            scheduler: Arc::new(scheduler),
            rss_fetch_job,
            logs,
            bangumi,
            cache,
        }
    }
}
