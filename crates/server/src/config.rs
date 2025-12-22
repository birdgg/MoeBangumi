use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub database_url: String,
    pub max_connections: u32,
}

impl Config {
    pub fn new(database_url: String) -> Self {
        Self {
            database_url,
            max_connections: 5,
        }
    }
}
