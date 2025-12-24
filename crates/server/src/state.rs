use bgmtv::BgmtvClient;
use mikan::MikanClient;
use reqwest::Client;
use sqlx::SqlitePool;
use std::sync::Arc;
use tmdb::TmdbClient;

use crate::config::Config;
use crate::services::SettingsService;

#[derive(Clone)]
pub struct AppState {
    pub db: SqlitePool,
    pub config: Arc<Config>,
    pub http_client: Client,
    pub tmdb: Arc<TmdbClient>,
    pub bgmtv: Arc<BgmtvClient>,
    pub mikan: Arc<MikanClient>,
    pub settings: Arc<SettingsService>,
}

impl AppState {
    pub fn new(db: SqlitePool, config: Config, settings: SettingsService) -> Self {
        let http_client = Client::new();
        let tmdb = TmdbClient::with_client(http_client.clone(), &config.tmdb_api_key);
        let bgmtv = BgmtvClient::with_client(http_client.clone());
        let mikan = MikanClient::new(http_client.clone());
        Self {
            db,
            config: Arc::new(config),
            http_client,
            tmdb: Arc::new(tmdb),
            bgmtv: Arc::new(bgmtv),
            mikan: Arc::new(mikan),
            settings: Arc::new(settings),
        }
    }
}
