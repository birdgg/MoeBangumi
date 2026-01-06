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
pub struct PaginatedResponse<T> {
    pub page: i64,
    pub results: Vec<T>,
    pub total_pages: i64,
    pub total_results: i64,
}
