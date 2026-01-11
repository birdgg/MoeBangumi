use serde::{Deserialize, Serialize};
// ===== Search/Discover Results =====

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TvShow {
    pub id: i64,
    pub name: String,
    pub original_name: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub backdrop_path: Option<String>,
    pub first_air_date: Option<String>,
    pub vote_average: f64,
    pub vote_count: i64,
    pub popularity: f64,
    pub genre_ids: Vec<i64>,
    pub origin_country: Vec<String>,
    pub original_language: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PaginatedResponse<T> {
    pub page: i64,
    pub results: Vec<T>,
    pub total_pages: i64,
    pub total_results: i64,
}

// ===== Common Types =====

/// Genre (movie/TV genre)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Genre {
    pub id: i64,
    pub name: String,
}

// ===== TV Series Types =====

/// TV Series Season (summary info from series detail)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Season {
    pub id: i64,
    pub name: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub season_number: i32,
    pub episode_count: i32,
    pub air_date: Option<String>,
}

/// TV Series Detail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TvSeriesDetail {
    pub id: i64,
    pub name: String,
    pub original_name: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub backdrop_path: Option<String>,
    pub first_air_date: Option<String>,
    pub number_of_seasons: i32,
    pub number_of_episodes: i32,
    pub seasons: Vec<Season>,
    pub genres: Vec<Genre>,
    pub vote_average: f64,
    pub status: String,
}

/// TV Episode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TvEpisode {
    pub id: i64,
    pub name: String,
    pub overview: String,
    pub episode_number: i32,
    pub season_number: i32,
    pub air_date: Option<String>,
    pub still_path: Option<String>,
    pub vote_average: f64,
}

/// Season Detail (with episodes list)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeasonDetail {
    pub id: i64,
    pub name: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub season_number: i32,
    pub episodes: Vec<TvEpisode>,
    pub air_date: Option<String>,
}

// ===== Movie Types =====

/// Movie Detail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MovieDetail {
    pub id: i64,
    pub title: String,
    pub original_title: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub backdrop_path: Option<String>,
    pub release_date: Option<String>,
    pub runtime: Option<i32>,
    pub genres: Vec<Genre>,
    pub vote_average: f64,
    pub status: String,
}

// ===== Search Multi Types =====

/// Media Type for search results
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MediaType {
    Tv,
    Movie,
    Person,
}

/// Search Result Item (unified for TV and Movie)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResultItem {
    pub media_type: MediaType,
    pub id: i64,

    // TV fields
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub original_name: Option<String>,
    #[serde(default)]
    pub first_air_date: Option<String>,

    // Movie fields
    #[serde(default)]
    pub title: Option<String>,
    #[serde(default)]
    pub original_title: Option<String>,
    #[serde(default)]
    pub release_date: Option<String>,

    // Common fields
    #[serde(default)]
    pub overview: Option<String>,
    pub poster_path: Option<String>,
    pub backdrop_path: Option<String>,
    #[serde(default)]
    pub vote_average: f64,
    #[serde(default)]
    pub popularity: f64,
}
