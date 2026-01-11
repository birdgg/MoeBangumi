use chrono::Datelike;
use parser::SubType;
use serde::{Deserialize, Serialize};
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeasonalBangumi {
    pub mikan_id: String,
    pub name: String,
    /// Air weekday (0=Sunday, 1=Monday, ..., 6=Saturday)
    pub air_week: i32,
    /// Poster image URL
    pub poster_url: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

    /// Get the current year and season
    pub fn current_year_season() -> (i32, Season) {
        let now = chrono::Utc::now();
        (now.year(), Season::current())
    }

    fn order(&self) -> u8 {
        match self {
            Season::Winter => 0,
            Season::Spring => 1,
            Season::Summer => 2,
            Season::Fall => 3,
        }
    }

    fn prev(&self) -> (i32, Season) {
        match self {
            Season::Winter => (-1, Season::Fall), // year offset
            Season::Spring => (0, Season::Winter),
            Season::Summer => (0, Season::Spring),
            Season::Fall => (0, Season::Summer),
        }
    }
}

/// Iterator that traverses seasons backwards from current to a target season
pub struct SeasonIterator {
    current_year: i32,
    current_season: Season,
    end_year: i32,
    end_season: Season,
    finished: bool,
}

impl SeasonIterator {
    /// Create iterator from current season to target (inclusive)
    pub fn from_current_to(end_year: i32, end_season: Season) -> Self {
        let (current_year, current_season) = Season::current_year_season();
        Self {
            current_year,
            current_season,
            end_year,
            end_season,
            finished: false,
        }
    }

    /// Create iterator from specified start to target (inclusive)
    pub fn new(start_year: i32, start_season: Season, end_year: i32, end_season: Season) -> Self {
        Self {
            current_year: start_year,
            current_season: start_season,
            end_year,
            end_season,
            finished: false,
        }
    }

    fn is_at_or_after_end(&self) -> bool {
        if self.current_year > self.end_year {
            true
        } else if self.current_year < self.end_year {
            false
        } else {
            self.current_season.order() >= self.end_season.order()
        }
    }
}

impl Iterator for SeasonIterator {
    type Item = (i32, Season);

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        if !self.is_at_or_after_end() {
            self.finished = true;
            return None;
        }

        let result = (self.current_year, self.current_season);

        if self.current_year == self.end_year && self.current_season == self.end_season {
            self.finished = true;
        } else {
            let (year_offset, prev_season) = self.current_season.prev();
            self.current_year += year_offset;
            self.current_season = prev_season;
        }

        Some(result)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BangumiDetail {
    pub bgmtv_id: Option<i64>,
    pub subgroups: Vec<Subgroup>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subgroup {
    pub id: String,
    pub name: String,
    pub rss_url: String,
    pub episodes: Vec<Episode>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Episode {
    pub name: String,
    pub torrent_url: Option<String>,
    // 解析后的元数据
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub subtitle_languages: Vec<SubType>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolution: Option<String>,
}
