use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

use super::Platform;

/// Calendar entry entity
/// Links a bangumi to a specific season (year + quarter)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Calendar {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Foreign key to bangumi
    pub bangumi_id: i64,
    /// Year (e.g., 2024)
    pub year: i32,
    /// Season (winter, spring, summer, fall)
    pub season: String,
    /// Display priority (based on BGM.tv collection_doing)
    pub priority: i32,
}

/// Subject item in calendar results
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CalendarSubject {
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,
    /// Mikan bangumi ID
    pub mikan_id: Option<String>,
    /// Chinese title
    pub title_chinese: String,
    /// Japanese title
    pub title_japanese: Option<String>,
    /// Season number from bangumi
    pub season: i32,
    /// First air date (YYYY-MM-DD)
    pub air_date: Option<String>,
    /// Day of week (0=Sunday, 1-6=Mon-Sat)
    pub air_week: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
    /// Platform type (tv, movie, ova)
    pub platform: Platform,
    /// Total episodes
    pub total_episodes: i32,
}

/// Weekday info
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Weekday {
    /// Weekday ID (1=Mon, 7=Sun)
    pub id: i32,
    /// English name
    pub en: String,
    /// Chinese name
    pub cn: String,
    /// Japanese name
    pub ja: String,
}

/// Calendar day with weekday info and items
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CalendarDay {
    pub weekday: Weekday,
    pub items: Vec<CalendarSubject>,
}

impl Weekday {
    pub fn from_id(id: i32) -> Self {
        let (en, cn, ja) = match id {
            1 => ("Mon", "星期一", "月曜日"),
            2 => ("Tue", "星期二", "火曜日"),
            3 => ("Wed", "星期三", "水曜日"),
            4 => ("Thu", "星期四", "木曜日"),
            5 => ("Fri", "星期五", "金曜日"),
            6 => ("Sat", "星期六", "土曜日"),
            7 => ("Sun", "星期日", "日曜日"),
            _ => ("Unknown", "未知", "不明"),
        };

        Self {
            id,
            en: en.to_string(),
            cn: cn.to_string(),
            ja: ja.to_string(),
        }
    }
}

/// Request body for creating a calendar entry
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CreateCalendar {
    /// Foreign key to bangumi
    pub bangumi_id: i64,
    /// Year (e.g., 2024)
    pub year: i32,
    /// Season (winter, spring, summer, fall)
    pub season: String,
    /// Display priority
    #[serde(default)]
    pub priority: i32,
}

/// Request body for updating a calendar entry
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateCalendar {
    /// Display priority
    #[serde(default)]
    pub priority: Option<i32>,
}
