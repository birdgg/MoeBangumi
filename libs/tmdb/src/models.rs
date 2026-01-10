use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
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
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Movie {
    pub id: i64,
    pub title: String,
    pub original_title: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub backdrop_path: Option<String>,
    pub release_date: Option<String>,
    pub vote_average: f64,
    pub vote_count: i64,
    pub popularity: f64,
    pub genre_ids: Vec<i64>,
    pub original_language: String,
    #[serde(default)]
    pub adult: bool,
    #[serde(default)]
    pub video: bool,
}

/// Search result from the multi search endpoint.
/// Can be either a TV show or a movie.
#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(tag = "media_type", rename_all = "snake_case")]
pub enum SearchMultiResult {
    Tv(TvShow),
    Movie(Movie),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct PaginatedResponse<T> {
    pub page: i64,
    pub results: Vec<T>,
    pub total_pages: i64,
    pub total_results: i64,
}

// ============ TV Show Details ============

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Genre {
    pub id: i64,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SeasonSummary {
    pub id: i64,
    pub name: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub season_number: i64,
    pub episode_count: i64,
    pub air_date: Option<String>,
    pub vote_average: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct TvShowDetails {
    pub id: i64,
    pub name: String,
    pub original_name: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub backdrop_path: Option<String>,
    pub first_air_date: Option<String>,
    pub last_air_date: Option<String>,
    pub vote_average: f64,
    pub vote_count: i64,
    pub popularity: f64,
    pub genres: Vec<Genre>,
    pub origin_country: Vec<String>,
    pub original_language: String,
    pub number_of_episodes: i64,
    pub number_of_seasons: i64,
    pub seasons: Vec<SeasonSummary>,
    pub status: String,
    #[serde(default)]
    pub in_production: bool,
    #[serde(default)]
    pub adult: bool,
}

// ============ Season Details ============

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Episode {
    pub id: i64,
    pub name: String,
    pub overview: String,
    pub episode_number: i64,
    pub season_number: i64,
    pub air_date: Option<String>,
    pub still_path: Option<String>,
    pub vote_average: f64,
    pub vote_count: i64,
    pub runtime: Option<i64>,
    pub show_id: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SeasonDetails {
    pub id: i64,
    #[serde(rename = "_id")]
    pub mongo_id: Option<String>,
    pub name: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub season_number: i64,
    pub air_date: Option<String>,
    pub vote_average: f64,
    pub episodes: Vec<Episode>,
}

// ============ Movie Details ============

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct MovieDetails {
    pub id: i64,
    pub title: String,
    pub original_title: String,
    pub overview: String,
    pub poster_path: Option<String>,
    pub backdrop_path: Option<String>,
    pub release_date: Option<String>,
    pub vote_average: f64,
    pub vote_count: i64,
    pub popularity: f64,
    pub genres: Vec<Genre>,
    pub original_language: String,
    #[serde(default)]
    pub adult: bool,
    #[serde(default)]
    pub video: bool,
    pub runtime: Option<i64>,
    pub status: Option<String>,
    pub tagline: Option<String>,
    pub budget: Option<i64>,
    pub revenue: Option<i64>,
    pub imdb_id: Option<String>,
    pub homepage: Option<String>,
}
