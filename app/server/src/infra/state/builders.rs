//! Builder functions for constructing application state components.
//!
//! This module contains the logic for building HTTP clients, API clients,
//! services, and actors.

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use bgmtv::BgmtvClient;
use metadata::{BgmtvProvider, MetadataClient, TmdbProvider};
use mikan::MikanClient;
use parking_lot::RwLock;
use rss::RssClient;
use sqlx::SqlitePool;
use tmdb::TmdbClient;
use updater::{UpdateConfig, UpdateService, UpdateServiceHandle};

use crate::config::Config;
use domain::services::actors::metadata::{create_metadata_actor, MetadataHandle, PosterService};
use domain::services::{
    create_downloader_service, create_notification_service, BangumiService, CacheService,
    CalendarService, HttpClientService, LogService, RenameService, RssProcessingService,
    ScanService, SettingsService, TorrentMetadataResolver, WashingService,
};
use jobs::{create_log_cleanup_actor, create_rename_actor, create_rss_fetch_actor};

use super::{AppActors, AppServices};

/// Type alias for the client provider closure
pub type ClientProvider = Arc<
    dyn Fn() -> std::pin::Pin<
            Box<
                dyn std::future::Future<
                        Output = Result<reqwest::Client, Box<dyn std::error::Error + Send + Sync>>,
                    > + Send,
            >,
        > + Send
        + Sync,
>;

/// Intermediate struct holding API clients created during initialization
pub struct ApiClients {
    pub tmdb: Arc<TmdbClient>,
    pub bgmtv: Arc<BgmtvClient>,
    pub mikan: Arc<MikanClient>,
    pub rss: Arc<RssClient>,
}

/// Create a client provider closure from HttpClientService
pub fn create_client_provider(http_service: Arc<HttpClientService>) -> ClientProvider {
    Arc::new(move || {
        let client = http_service.get_client();
        Box::pin(async move { Ok(client) })
    })
}

/// Spawn a background task to watch for TMDB API key changes
pub fn spawn_tmdb_api_key_watcher(settings: &Arc<SettingsService>, tmdb_api_key: tmdb::ApiKey) {
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
}

/// Build HTTP client service with dynamic proxy support
pub fn build_http_client(settings: &Arc<SettingsService>) -> Arc<HttpClientService> {
    Arc::new(
        HttpClientService::new(Arc::clone(settings)).expect("Failed to create HTTP client service"),
    )
}

/// Build all external API clients
pub fn build_api_clients(
    settings: &Arc<SettingsService>,
    http_client: &Arc<HttpClientService>,
    client_provider: &ClientProvider,
) -> ApiClients {
    // TMDB client with API key watcher
    let tmdb_api_key: tmdb::ApiKey = Arc::new(RwLock::new(settings.get().tmdb.api_key.clone()));
    let tmdb = Arc::new(TmdbClient::new(
        http_client.get_client(),
        Arc::clone(&tmdb_api_key),
    ));
    spawn_tmdb_api_key_watcher(settings, tmdb_api_key);

    // Other API clients
    let bgmtv = Arc::new(BgmtvClient::new(http_client.get_client()));
    let mikan = Arc::new(MikanClient::with_client_provider(Arc::clone(
        client_provider,
    )));
    let rss = Arc::new(RssClient::with_client_provider(Arc::clone(client_provider)));

    ApiClients {
        tmdb,
        bgmtv,
        mikan,
        rss,
    }
}

/// Build all application services
/// Returns (services, metadata_actor, rss_processing) for actor creation
#[allow(clippy::too_many_arguments)]
pub fn build_services(
    db: &SqlitePool,
    config: &Arc<Config>,
    settings: &Arc<SettingsService>,
    http_client: &Arc<HttpClientService>,
    client_provider: &ClientProvider,
    api_clients: &ApiClients,
    current_version: &str,
) -> (AppServices, Arc<MetadataHandle>, Arc<RssProcessingService>) {
    // Core services
    let logs = Arc::new(LogService::new(db.clone()));
    let downloader = Arc::new(create_downloader_service(Arc::clone(settings)));
    let cache = Arc::new(CacheService::new(db.clone()));

    // Poster and metadata services
    let poster = Arc::new(PosterService::with_client_provider(
        Arc::clone(client_provider),
        config.posters_path(),
    ));
    let metadata_client = Arc::new(MetadataClient::new(
        BgmtvProvider::new(Arc::clone(&api_clients.bgmtv)),
        TmdbProvider::new(Arc::clone(&api_clients.tmdb)),
    ));
    let metadata_actor = Arc::new(create_metadata_actor(db.clone(), Arc::clone(&poster)));

    // RSS and washing services
    let washing = Arc::new(WashingService::new(
        db.clone(),
        Arc::clone(&downloader),
        Arc::clone(settings),
    ));
    let rss_processing = Arc::new(RssProcessingService::new(
        db.clone(),
        Arc::clone(&api_clients.rss),
        Arc::clone(&downloader),
        Arc::clone(settings),
        Arc::clone(&washing),
    ));

    // Bangumi and notification services
    let bangumi = Arc::new(BangumiService::new(
        db.clone(),
        Arc::clone(&rss_processing),
        Arc::clone(settings),
    ));
    let notification = Arc::new(create_notification_service(
        Arc::clone(settings),
        Arc::clone(http_client),
    ));

    // Torrent metadata resolver for untracked torrents
    let resolver = Arc::new(TorrentMetadataResolver::new(
        db.clone(),
        Arc::clone(&metadata_client),
        Arc::clone(settings),
    ));

    // Rename and calendar services
    let rename = Arc::new(
        RenameService::new(
            db.clone(),
            Arc::clone(&downloader),
            Arc::clone(&notification),
            Arc::clone(config),
        )
        .with_resolver(resolver),
    );
    let calendar = Arc::new(CalendarService::new(
        db.clone(),
        Arc::clone(&metadata_actor),
    ));

    // Scan service
    let scan = Arc::new(ScanService::new(
        db.clone(),
        Arc::clone(&bangumi),
        Arc::clone(&metadata_client),
        Arc::clone(settings),
    ));

    // Update service
    let update = build_update_service(http_client, current_version);

    let services = AppServices {
        metadata_client,
        bangumi,
        calendar,
        cache,
        logs,
        downloader,
        poster,
        notification,
        rename,
        scan,
        update,
    };

    (services, metadata_actor, rss_processing)
}

/// Build the update service
pub fn build_update_service(
    http_client: &Arc<HttpClientService>,
    current_version: &str,
) -> UpdateServiceHandle {
    let update_config = UpdateConfig::new("birdgg", "moe-bangumi", current_version)
        .bin_name("moe")
        .target_dir(PathBuf::from("/data/bin"))
        .check_interval(Duration::from_secs(86400))
        .auto_check(true);

    let (handle, service) = UpdateService::new(update_config, http_client.get_client());
    tokio::spawn(service.run());
    handle
}

/// Build and start background actors
pub fn build_actors(
    db: &SqlitePool,
    settings: &Arc<SettingsService>,
    services: &AppServices,
    metadata_actor: Arc<MetadataHandle>,
    rss_processing: Arc<RssProcessingService>,
) -> AppActors {
    let rss_fetch = create_rss_fetch_actor(db.clone(), rss_processing, Arc::clone(settings));
    let log_cleanup = create_log_cleanup_actor(Arc::clone(&services.logs));
    let rename = create_rename_actor(Arc::clone(&services.rename), Arc::clone(settings));

    AppActors {
        metadata: metadata_actor,
        rss_fetch,
        rename,
        log_cleanup,
    }
}
