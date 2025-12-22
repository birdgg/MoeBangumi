use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub database_url: String,
    pub max_connections: u32,
    pub tmdb_api_key: String,
}

impl Config {
    pub fn new(database_url: String, tmdb_api_key: String) -> Self {
        Self {
            database_url,
            max_connections: 5,
            tmdb_api_key,
        }
    }
}
