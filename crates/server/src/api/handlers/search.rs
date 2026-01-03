use axum::{
    extract::{Query, State},
    Json,
};

use crate::error::AppResult;
use crate::state::AppState;
use tmdb::DiscoverBangumiParams;

use super::{SearchQuery, TmdbSearchQuery, MIKAN_SEARCH_CACHE_TTL};

/// Search for bangumi (Japanese anime) on BGM.tv
#[utoipa::path(
    get,
    path = "/api/search/bgmtv",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results", body = Vec<bgmtv::ParsedSubject>)
    )
)]
pub async fn search_bgmtv(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<Vec<bgmtv::ParsedSubject>>> {
    let results = state.bgmtv.search_bangumi(&query.keyword).await?;
    Ok(Json(results))
}

/// Search for anime on TMDB using discover API
#[utoipa::path(
    get,
    path = "/api/search/tmdb",
    tag = "search",
    params(TmdbSearchQuery),
    responses(
        (status = 200, description = "Search results from TMDB", body = Vec<tmdb::models::TvShow>)
    )
)]
pub async fn search_tmdb(
    State(state): State<AppState>,
    Query(query): Query<TmdbSearchQuery>,
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
        (status = 200, description = "Search results from Mikan", body = Vec<mikan::SearchResult>)
    )
)]
pub async fn search_mikan(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<Vec<mikan::SearchResult>>> {
    let cache_key = format!("mikan:search:{}", query.keyword);
    let mikan = state.mikan.clone();
    let keyword = query.keyword.clone();

    let results = state
        .cache
        .get_or_fetch(&cache_key, MIKAN_SEARCH_CACHE_TTL, || async move {
            mikan.search_bangumi(&keyword).await
        })
        .await?;

    Ok(Json(results))
}
