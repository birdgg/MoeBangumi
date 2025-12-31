use bgmtv::BgmtvClient;
use mikan::MikanClient;
use rss::RssClient;
use sqlx::SqlitePool;
use std::sync::Arc;
use tmdb::TmdbClient;

use crate::config::Config;
use crate::services::{
    BangumiService, CacheService, CalendarRefreshJob, CalendarService, DownloaderService,
    HttpClientService, LogCleanupJob, LogService, MetadataService, MikanMappingService,
    MikanMappingSyncJob, NotificationService, PosterService, RenameJob, RenameService,
    RssFetchJob, RssProcessingService, SchedulerService, SettingsService, TorrentSearchService,
    WashingService,
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
    pub metadata: Arc<MetadataService>,
    pub bangumi: Arc<BangumiService>,
    pub cache: Arc<CacheService>,
    pub calendar: Arc<CalendarService>,
    pub torrent_search: Arc<TorrentSearchService>,
    pub notification: Arc<NotificationService>,
    pub rename: Arc<RenameService>,
    pub mikan_mapping: Arc<MikanMappingService>,
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

        // Create washing service (for priority-based torrent replacement)
        let washing = Arc::new(WashingService::new(
            db.clone(),
            Arc::clone(&downloader_arc),
            Arc::clone(&settings),
        ));

        // Create RSS processing service (shared by BangumiService and RssFetchJob)
        let rss_processing = Arc::new(RssProcessingService::new(
            db.clone(),
            Arc::clone(&rss_arc),
            Arc::clone(&downloader_arc),
            Arc::clone(&settings),
            Arc::clone(&washing),
        ));

        // Create metadata service
        let metadata = Arc::new(MetadataService::new(db.clone()));

        // Create bangumi service (with MetadataService and RSS processing for immediate fetch)
        let bangumi = Arc::new(BangumiService::new(
            db.clone(),
            Arc::clone(&metadata),
            Arc::clone(&poster),
            Arc::clone(&rss_processing),
            Arc::clone(&settings),
        ));

        // Create RSS fetch job (stored separately for manual triggering)
        let rss_fetch_job = Arc::new(RssFetchJob::new(
            db.clone(),
            Arc::clone(&rss_processing),
        ));

        // Create notification service
        let notification = Arc::new(NotificationService::new(
            Arc::clone(&settings),
            Arc::clone(&http_client_service),
        ));

        // Create rename service (for file renaming to Plex/Jellyfin format)
        let rename = Arc::new(RenameService::new(
            db.clone(),
            Arc::clone(&downloader_arc),
        ));

        // Create calendar service (for BGM.tv weekly schedule)
        let bgmtv_arc = Arc::new(bgmtv);
        let calendar = Arc::new(CalendarService::new(db.clone(), Arc::clone(&bgmtv_arc)));

        // Create Mikan mapping service (for Mikan-BGM.tv ID mapping)
        let mikan_arc = Arc::new(mikan);
        let mikan_mapping = Arc::new(MikanMappingService::new(
            db.clone(),
            Arc::clone(&mikan_arc),
        ));

        // Create and start scheduler service
        let scheduler = SchedulerService::new()
            .with_arc_job(Arc::clone(&rss_fetch_job))
            .with_job(LogCleanupJob::new(Arc::clone(&logs)))
            .with_job(RenameJob::new(Arc::clone(&rename)))
            .with_job(CalendarRefreshJob::new(Arc::clone(&calendar)))
            .with_job(MikanMappingSyncJob::new(Arc::clone(&mikan_mapping)));
        scheduler.start();

        Self {
            db,
            config: Arc::new(config),
            http_client_service,
            tmdb: Arc::new(tmdb),
            bgmtv: bgmtv_arc,
            mikan: mikan_arc,
            rss: rss_arc,
            settings,
            downloader: downloader_arc,
            poster,
            scheduler: Arc::new(scheduler),
            rss_fetch_job,
            logs,
            metadata,
            bangumi,
            cache,
            calendar,
            torrent_search,
            notification,
            rename,
            mikan_mapping,
        }
    }
}
