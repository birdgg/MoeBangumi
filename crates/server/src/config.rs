use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum Environment {
    #[default]
    Dev,
    Prod,
}

impl Environment {
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "prod" | "production" => Self::Prod,
            _ => Self::Dev,
        }
    }

    /// Returns the default data path for this environment
    pub fn default_data_path(&self) -> PathBuf {
        match self {
            Self::Dev => PathBuf::from("./data"),
            Self::Prod => PathBuf::from("/data"),
        }
    }

    pub fn is_dev(&self) -> bool {
        matches!(self, Self::Dev)
    }

    pub fn is_prod(&self) -> bool {
        matches!(self, Self::Prod)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub env: Environment,
    pub data_path: PathBuf,
    pub database_url: String,
    pub max_connections: u32,
    pub tmdb_api_key: String,
}

impl Config {
    pub fn new(env: Environment, data_path: impl AsRef<Path>, tmdb_api_key: String) -> Self {
        let data_path = data_path.as_ref().to_path_buf();
        let database_url = format!(
            "sqlite:{}?mode=rwc",
            data_path.join("moe.db").display()
        );
        Self {
            env,
            data_path,
            database_url,
            max_connections: 5,
            tmdb_api_key,
        }
    }

    /// Returns the path to the posters directory
    pub fn posters_path(&self) -> PathBuf {
        self.data_path.join("posters")
    }

    /// Returns the path to the settings TOML file
    pub fn settings_path(&self) -> PathBuf {
        self.data_path.join("settings.toml")
    }
}
