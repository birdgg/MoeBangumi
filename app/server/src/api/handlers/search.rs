use axum::{
    extract::{Query, State},
    Json,
};
use metadata::{
    CombinedSearchResults, MetadataSource, SearchQuery as MetadataSearchQuery, SearchedMetadata,
};
use serde::Deserialize;

use crate::error::AppResult;
use crate::state::AppState;

use super::{SearchQuery, TmdbSearchQuery, MIKAN_SEARCH_CACHE_TTL};

/// Query parameters for unified metadata search
#[derive(Debug, Deserialize)]
pub struct UnifiedSearchQuery {
    pub source: MetadataSource,
    pub keyword: String,
    pub year: Option<i32>,
}

/// Query parameters for metadata detail lookup
#[derive(Debug, Deserialize)]
pub struct DetailQuery {
    pub source: MetadataSource,
    pub external_id: String,
}

pub async fn search_bgmtv(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<Vec<SearchedMetadata>>> {
    let search_query = MetadataSearchQuery::new(&query.keyword);
    let results = state
        .services
        .metadata_client
        .search(&search_query, MetadataSource::Bgmtv)
        .await?;
    Ok(Json(results))
}

pub async fn search_tmdb(
    State(state): State<AppState>,
    Query(query): Query<TmdbSearchQuery>,
) -> AppResult<Json<Vec<SearchedMetadata>>> {
    let search_query = MetadataSearchQuery::new(&query.keyword);
    let results = state
        .services
        .metadata_client
        .search(&search_query, MetadataSource::Tmdb)
        .await?;
    Ok(Json(results))
}

pub async fn search_mikan(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<Vec<mikan::SearchResult>>> {
    let cache_key = format!("mikan:search:{}", query.keyword);
    let mikan = state.clients.mikan.clone();
    let keyword = query.keyword.clone();

    let results = state
        .services
        .cache
        .get_or_fetch(&cache_key, MIKAN_SEARCH_CACHE_TTL, || async move {
            mikan.search_bangumi(&keyword).await
        })
        .await?;

    Ok(Json(results))
}

pub async fn search_metadata(
    State(state): State<AppState>,
    Query(query): Query<UnifiedSearchQuery>,
) -> AppResult<Json<Vec<SearchedMetadata>>> {
    let search_query = MetadataSearchQuery::new(&query.keyword);
    let results = state
        .services
        .metadata_client
        .search(&search_query, query.source)
        .await?;
    Ok(Json(results))
}

pub async fn find_metadata(
    State(state): State<AppState>,
    Query(query): Query<UnifiedSearchQuery>,
) -> AppResult<Json<Option<SearchedMetadata>>> {
    let mut search_query = MetadataSearchQuery::new(&query.keyword);
    if let Some(year) = query.year {
        search_query = search_query.with_year(year);
    }
    let result = state
        .services
        .metadata_client
        .find(&search_query, query.source)
        .await?;
    Ok(Json(result))
}

pub async fn get_metadata_detail(
    State(state): State<AppState>,
    Query(query): Query<DetailQuery>,
) -> AppResult<Json<Option<SearchedMetadata>>> {
    let result = state
        .services
        .metadata_client
        .get_detail(&query.external_id, query.source)
        .await?;
    Ok(Json(result))
}

pub async fn search_metadata_all(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<CombinedSearchResults>> {
    let search_query = MetadataSearchQuery::new(&query.keyword);
    let results = state.services.metadata_client.search_all(&search_query).await;
    Ok(Json(results))
}
