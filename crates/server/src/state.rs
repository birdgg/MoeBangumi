use bgmtv::BgmtvClient;
use reqwest::Client;
use sqlx::SqlitePool;
use std::sync::Arc;
use tmdb::TmdbClient;

use crate::config::Config;

#[derive(Clone)]
pub struct AppState {
    pub db: SqlitePool,
    pub config: Arc<Config>,
    pub http_client: Client,
    pub tmdb: Arc<TmdbClient>,
    pub bgmtv: Arc<BgmtvClient>,
}

impl AppState {
    pub fn new(db: SqlitePool, config: Config) -> Self {
        let http_client = Client::new();
        let tmdb = TmdbClient::with_client(http_client.clone(), &config.tmdb_api_key);
        let bgmtv = BgmtvClient::with_client(http_client.clone());
        Self {
            db,
            config: Arc::new(config),
            http_client,
            tmdb: Arc::new(tmdb),
            bgmtv: Arc::new(bgmtv),
        }
    }
}
