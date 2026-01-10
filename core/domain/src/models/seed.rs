use serde::{Deserialize, Serialize};

/// Seed entry for a single bangumi (used for calendar seed data)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeedEntry {
    /// Mikan bangumi ID
    pub mikan_id: Option<String>,
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,
    /// TMDB ID (for series)
    pub tmdb_id: Option<i64>,
    /// Chinese title
    pub title_chinese: String,
    /// Japanese title
    pub title_japanese: Option<String>,
    /// Season number
    #[serde(default = "default_season")]
    pub season: i32,
    /// Year
    pub year: i32,
    /// Platform (tv, movie, ova)
    #[serde(default = "default_platform")]
    pub platform: String,
    /// Total episodes
    #[serde(default)]
    pub total_episodes: i32,
    /// Poster URL
    pub poster_url: Option<String>,
    /// First air date (YYYY-MM-DD)
    pub air_date: Option<String>,
    /// Day of week (0=Sunday, 1-6=Mon-Sat)
    #[serde(default)]
    pub air_week: i32,
}

fn default_season() -> i32 {
    1
}

fn default_platform() -> String {
    "tv".to_string()
}

/// Data for a single season (used for seed data import/export)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeasonData {
    pub year: i32,
    pub season: String,
    pub entries: Vec<SeedEntry>,
}
