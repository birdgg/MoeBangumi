use axum::{
    extract::{Query, State},
    Json,
};

use crate::error::AppResult;
use crate::repositories::CacheRepository;
use crate::state::AppState;
use tmdb::DiscoverBangumiParams;

use super::{SearchQuery, MIKAN_SEARCH_CACHE_TTL};

/// Search for bangumi (Japanese anime) on BGM.tv
#[utoipa::path(
    get,
    path = "/api/search/bgmtv",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results", body = Vec<bgmtv::Subject>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_bgmtv(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<Vec<bgmtv::Subject>>> {
    let response = state.bgmtv.search_bangumi(&query.keyword).await?;
    Ok(Json(response.data))
}

/// Search for anime on TMDB using discover API
#[utoipa::path(
    get,
    path = "/api/search/tmdb",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results from TMDB", body = Vec<tmdb::models::TvShow>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_tmdb(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<Vec<tmdb::models::TvShow>>> {
    let params = DiscoverBangumiParams {
        with_text_query: Some(query.keyword),
    };
    let response = state.tmdb.discover_bangumi(params).await?;
    Ok(Json(response.results))
}

/// Search for bangumi on Mikan
#[utoipa::path(
    get,
    path = "/api/search/mikan",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results from Mikan", body = Vec<mikan::SearchResult>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_mikan(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<Vec<mikan::SearchResult>>> {
    let cache_key = format!("mikan:search:{}", query.keyword);

    // Try cache first
    if let Ok(Some(cached)) = CacheRepository::get::<Vec<mikan::SearchResult>>(
        &state.db,
        &cache_key,
        MIKAN_SEARCH_CACHE_TTL,
    )
    .await
    {
        return Ok(Json(cached));
    }

    // Fetch from Mikan
    let results = state.mikan.search_bangumi(&query.keyword).await?;

    // Cache the results
    if let Err(e) = CacheRepository::set(&state.db, &cache_key, &results).await {
        tracing::warn!("Failed to cache Mikan search results: {}", e);
    }

    Ok(Json(results))
}
