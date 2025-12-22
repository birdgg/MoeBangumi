use sqlx::SqlitePool;
use std::sync::Arc;

use crate::config::Config;

#[derive(Clone)]
pub struct AppState {
    pub db: SqlitePool,
    pub config: Arc<Config>,
}

impl AppState {
    pub fn new(db: SqlitePool, config: Config) -> Self {
        Self {
            db,
            config: Arc::new(config),
        }
    }
}
