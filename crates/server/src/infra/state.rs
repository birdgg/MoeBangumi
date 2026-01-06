use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use bgmtv::BgmtvClient;
use mikan::MikanClient;
use metadata::{BgmtvProvider, TmdbProvider};
use parking_lot::RwLock;
use sqlx::SqlitePool;
use tmdb::TmdbClient;
use updater::{UpdateConfig, UpdateService, UpdateServiceHandle};

use crate::config::Config;
use crate::metadata_service::{create_metadata_actor, MetadataHandle, MetadataService, PosterService};
use crate::rss::RssClient;
use crate::services::{
    create_downloader_service, create_log_cleanup_actor, create_notification_service,
    create_rename_actor, create_rss_fetch_actor, BangumiService, CacheService, CalendarService,
    DownloaderHandle, HttpClientService, LogCleanupHandle, LogService, NotificationService,
    RenameHandle, RenameService, RssFetchHandle, RssProcessingService, SettingsService,
    WashingService,
};

#[derive(Clone)]
pub struct AppState {
    pub db: SqlitePool,
    pub config: Arc<Config>,
    pub http_client_service: Arc<HttpClientService>,
    pub mikan: Arc<MikanClient>,
    pub rss: Arc<RssClient>,
    pub settings: Arc<SettingsService>,
    pub downloader: Arc<DownloaderHandle>,
    pub poster: Arc<PosterService>,
    pub metadata_actor: Arc<MetadataHandle>,
    pub logs: Arc<LogService>,
    pub metadata: Arc<MetadataService>,
    pub bangumi: Arc<BangumiService>,
    pub cache: Arc<CacheService>,
    pub calendar: Arc<CalendarService>,
    pub notification: Arc<NotificationService>,
    pub rename: Arc<RenameService>,
    pub update: UpdateServiceHandle,
    // Background actors (keep handles alive to prevent actors from stopping)
    #[allow(dead_code)]
    rss_fetch_actor: RssFetchHandle,
    #[allow(dead_code)]
    rename_actor: RenameHandle,
    #[allow(dead_code)]
    log_cleanup_actor: LogCleanupHandle,
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
    pub fn new(db: SqlitePool, config: Config, settings: SettingsService, current_version: &str) -> Self {
        // Wrap settings in Arc first (needed by HttpClientService and DownloaderService)
        let settings = Arc::new(settings);

        // Create HTTP client service with dynamic proxy support
        let http_client_service = Arc::new(
            HttpClientService::new(Arc::clone(&settings))
                .expect("Failed to create HTTP client service"),
        );
        let client_provider = create_client_provider(Arc::clone(&http_client_service));

        // Create API clients
        let tmdb_api_key: tmdb::ApiKey = Arc::new(RwLock::new(settings.get().tmdb.api_key.clone()));
        let tmdb = Arc::new(TmdbClient::new(http_client_service.get_client(), Arc::clone(&tmdb_api_key)));

        // Spawn background task to watch for TMDB API key changes
        let tmdb_api_key_clone = Arc::clone(&tmdb_api_key);
        let mut tmdb_watcher = settings.subscribe();
        tokio::spawn(async move {
            loop {
                if tmdb_watcher.changed().await.is_err() {
                    break;
                }
                let new_settings = tmdb_watcher.borrow().clone();
                let new_api_key = new_settings.tmdb.api_key;

                let should_update = {
                    let current = tmdb_api_key_clone.read();
                    *current != new_api_key
                };

                if should_update {
                    *tmdb_api_key_clone.write() = new_api_key;
                    tracing::info!("TMDB API key updated");
                }
            }
        });

        let bgmtv = Arc::new(BgmtvClient::new(http_client_service.get_client()));
        let mikan = MikanClient::with_client_provider(Arc::clone(&client_provider));
        let rss = RssClient::with_client_provider(Arc::clone(&client_provider));

        // Create log service for logging and notifications
        let logs = Arc::new(LogService::new(db.clone()));

        // Create downloader service with settings reference (Actor mode)
        let downloader = create_downloader_service(Arc::clone(&settings));

        // Create poster service with dynamic client provider
        let poster = Arc::new(PosterService::with_client_provider(
            Arc::clone(&client_provider),
            config.posters_path(),
        ));

        // Create unified metadata providers
        let bgmtv_provider = Arc::new(BgmtvProvider::new(Arc::clone(&bgmtv)));
        let tmdb_provider = Arc::new(TmdbProvider::new(Arc::clone(&tmdb)));

        // Create metadata service (using providers instead of clients)
        let metadata = Arc::new(MetadataService::new(
            db.clone(),
            Arc::clone(&bgmtv_provider),
            Arc::clone(&tmdb_provider),
        ));

        // Create metadata actor (handles poster downloads with internal scheduling)
        let metadata_actor = Arc::new(create_metadata_actor(
            db.clone(),
            Arc::clone(&poster),
        ));

        // Create cache service
        let cache = Arc::new(CacheService::new(db.clone()));

        // Create shared Arc references for scheduler jobs
        let rss_arc = Arc::new(rss);
        let downloader_arc = Arc::new(downloader);

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

        // Create bangumi service (with MetadataService and RSS processing for immediate fetch)
        let bangumi = Arc::new(BangumiService::new(
            db.clone(),
            Arc::clone(&metadata),
            Arc::clone(&rss_processing),
            Arc::clone(&settings),
        ));

        // Create notification service (Actor mode)
        let notification = Arc::new(create_notification_service(
            Arc::clone(&settings),
            Arc::clone(&http_client_service),
        ));

        // Wrap config in Arc for sharing
        let config = Arc::new(config);

        // Create rename service (for file renaming to Plex/Jellyfin format)
        let rename = Arc::new(RenameService::new(
            db.clone(),
            Arc::clone(&downloader_arc),
            Arc::clone(&notification),
            Arc::clone(&config),
        ));

        // Create Mikan client Arc (shared by calendar)
        let mikan_arc = Arc::new(mikan);

        // Create calendar service (fetches from Mikan -> BGM.tv)
        let calendar = Arc::new(CalendarService::new(
            db.clone(),
            Arc::clone(&metadata),
            Arc::clone(&mikan_arc),
            Arc::clone(&metadata_actor),
        ));

        // Create update service (reuse HTTP client for proxy support)
        let update_config = UpdateConfig::new("birdgg", "moe-bangumi", current_version)
            .bin_name("moe")
            .target_dir(PathBuf::from("/data/bin"))
            .check_interval(Duration::from_secs(86400)) // 24 hours
            .auto_check(true);

        let (update_handle, update_service) =
            UpdateService::new(update_config, http_client_service.get_client());

        // Spawn update service actor
        tokio::spawn(update_service.run());

        // Start background actors
        // Note: These actors have internal timers and run independently
        let rss_fetch_actor = create_rss_fetch_actor(db.clone(), Arc::clone(&rss_processing));
        let log_cleanup_actor = create_log_cleanup_actor(Arc::clone(&logs));
        let rename_actor = create_rename_actor(Arc::clone(&rename));

        Self {
            db,
            config,
            http_client_service,
            mikan: mikan_arc,
            rss: rss_arc,
            settings,
            downloader: downloader_arc,
            poster,
            metadata_actor,
            logs,
            metadata,
            bangumi,
            cache,
            calendar,
            notification,
            rename,
            update: update_handle,
            rss_fetch_actor,
            rename_actor,
            log_cleanup_actor,
        }
    }
}
