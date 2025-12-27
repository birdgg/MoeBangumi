use bgmtv::BgmtvClient;
use mikan::MikanClient;
use rss::RssClient;
use sqlx::SqlitePool;
use std::sync::Arc;
use tmdb::TmdbClient;

use crate::config::Config;
use crate::services::{
    BangumiService, CacheService, DownloaderService, FileRenameJob, HttpClientService,
    LogCleanupJob, LogService, PosterService, RssFetchJob, RssProcessingService,
    SchedulerService, SettingsService, TorrentSearchService,
};

#[derive(Clone)]
pub struct AppState {
    pub db: SqlitePool,
    pub config: Arc<Config>,
    pub http_client_service: Arc<HttpClientService>,
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
    pub torrent_search: Arc<TorrentSearchService>,
}

/// Create a client provider closure from HttpClientService
fn create_client_provider(
    http_service: Arc<HttpClientService>,
) -> Arc<
    dyn Fn() -> std::pin::Pin<
            Box<
                dyn std::future::Future<
                        Output = Result<reqwest::Client, Box<dyn std::error::Error + Send + Sync>>,
                    > + Send,
            >,
        > + Send
        + Sync,
> {
    Arc::new(move || {
        // get_client() is now sync, wrap in async for compatibility with ClientProvider type
        let client = http_service.get_client();
        Box::pin(async move { Ok(client) })
    })
}

impl AppState {
    pub fn new(db: SqlitePool, config: Config, settings: SettingsService) -> Self {
        // Wrap settings in Arc first (needed by HttpClientService and DownloaderService)
        let settings = Arc::new(settings);

        // Create HTTP client service with dynamic proxy support
        let http_client_service = Arc::new(
            HttpClientService::new(Arc::clone(&settings))
                .expect("Failed to create HTTP client service"),
        );
        let client_provider = create_client_provider(Arc::clone(&http_client_service));

        // Create API clients with dynamic client provider
        let tmdb = TmdbClient::with_client_provider(Arc::clone(&client_provider), &config.tmdb_api_key);
        let bgmtv = BgmtvClient::with_client_provider(Arc::clone(&client_provider));
        let mikan = MikanClient::with_client_provider(Arc::clone(&client_provider));
        let rss = RssClient::with_client_provider(Arc::clone(&client_provider));

        // Create log service for logging and notifications
        let logs = Arc::new(LogService::new(db.clone()));

        // Create downloader service with settings reference
        let downloader = DownloaderService::new(Arc::clone(&settings));

        // Create poster service with dynamic client provider
        let poster = Arc::new(PosterService::with_client_provider(
            Arc::clone(&client_provider),
            config.posters_path(),
        ));

        // Create cache service
        let cache = Arc::new(CacheService::new(db.clone()));

        // Create shared Arc references for scheduler jobs
        let rss_arc = Arc::new(rss);
        let downloader_arc = Arc::new(downloader);

        // Create torrent search service
        let torrent_search = Arc::new(TorrentSearchService::new(Arc::clone(&rss_arc)));

        // Create RSS processing service (shared by BangumiService and RssFetchJob)
        let rss_processing = Arc::new(RssProcessingService::new(
            db.clone(),
            Arc::clone(&rss_arc),
            Arc::clone(&downloader_arc),
            Arc::clone(&settings),
        ));

        // Create bangumi service (with RSS processing for immediate fetch)
        let bangumi = Arc::new(BangumiService::new(
            db.clone(),
            Arc::clone(&poster),
            Arc::clone(&rss_processing),
            Arc::clone(&settings),
        ));

        // Create RSS fetch job (stored separately for manual triggering)
        let rss_fetch_job = Arc::new(RssFetchJob::new(
            db.clone(),
            Arc::clone(&rss_processing),
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
            http_client_service,
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
            torrent_search,
        }
    }
}
