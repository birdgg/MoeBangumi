use chrono::Datelike;
use parser::SubType;
use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SearchResult {
    pub id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SeasonalBangumi {
    pub mikan_id: String,
    pub name: String,
    /// Air weekday (0=Sunday, 1=Monday, ..., 6=Saturday)
    pub air_week: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum Season {
    Winter,
    Spring,
    Summer,
    Fall,
}

impl Season {
    pub fn to_chinese(&self) -> &'static str {
        match self {
            Season::Winter => "冬",
            Season::Spring => "春",
            Season::Summer => "夏",
            Season::Fall => "秋",
        }
    }

    pub fn to_db_string(&self) -> &'static str {
        match self {
            Season::Winter => "winter",
            Season::Spring => "spring",
            Season::Summer => "summer",
            Season::Fall => "fall",
        }
    }

    pub fn current() -> Self {
        let month = chrono::Utc::now().month();
        match month {
            1..=3 => Season::Winter,
            4..=6 => Season::Spring,
            7..=9 => Season::Summer,
            _ => Season::Fall,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct BangumiDetail {
    pub bgmtv_id: Option<i64>,
    pub subgroups: Vec<Subgroup>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Subgroup {
    pub id: String,
    pub name: String,
    pub rss_url: String,
    pub episodes: Vec<Episode>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Episode {
    pub name: String,
    pub torrent_url: Option<String>,
    // 解析后的元数据
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub subtitle_languages: Vec<SubType>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolution: Option<String>,
}
