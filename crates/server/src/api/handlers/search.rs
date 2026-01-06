use axum::{
    extract::{Query, State},
    Json,
};
use metadata::{MetadataSource, SearchQuery as MetadataSearchQuery, SearchedMetadata};
use serde::Deserialize;
#[cfg(feature = "openapi")]
use utoipa::{IntoParams, ToSchema};

use crate::error::AppResult;
use crate::state::AppState;

use super::{SearchQuery, TmdbSearchQuery, MIKAN_SEARCH_CACHE_TTL};

/// Query parameters for unified metadata search
#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "openapi", derive(IntoParams, ToSchema))]
pub struct UnifiedSearchQuery {
    /// Data source to search (bgmtv, tmdb)
    pub source: MetadataSource,
    /// Search keyword
    pub keyword: String,
    /// Optional year filter
    pub year: Option<i32>,
}

/// Query parameters for metadata detail lookup
#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "openapi", derive(IntoParams, ToSchema))]
pub struct DetailQuery {
    /// Data source (bgmtv, tmdb)
    pub source: MetadataSource,
    /// External ID from the data source
    pub external_id: String,
}

/// Search for bangumi (Japanese anime) on BGM.tv
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/search/bgmtv",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results", body = Vec<SearchedMetadata>)
    )
))]
pub async fn search_bgmtv(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> AppResult<Json<Vec<SearchedMetadata>>> {
    let search_query = MetadataSearchQuery::new(&query.keyword);
    let results = state
        .metadata
        .search_provider(&search_query, MetadataSource::Bgmtv)
        .await?;
    Ok(Json(results))
}

/// Search for anime on TMDB using discover API
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/search/tmdb",
    tag = "search",
    params(TmdbSearchQuery),
    responses(
        (status = 200, description = "Search results from TMDB", body = Vec<SearchedMetadata>)
    )
))]
pub async fn search_tmdb(
    State(state): State<AppState>,
    Query(query): Query<TmdbSearchQuery>,
) -> AppResult<Json<Vec<SearchedMetadata>>> {
    let search_query = MetadataSearchQuery::new(&query.keyword);
    let results = state
        .metadata
        .search_provider(&search_query, MetadataSource::Tmdb)
        .await?;
    Ok(Json(results))
}

/// Search for bangumi on Mikan
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/search/mikan",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results from Mikan", body = Vec<mikan::SearchResult>)
    )
))]
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

/// Unified metadata search across all data sources
///
/// Returns standardized SearchedMetadata format regardless of data source.
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/search/metadata",
    tag = "search",
    params(UnifiedSearchQuery),
    responses(
        (status = 200, description = "Search results in unified format", body = Vec<SearchedMetadata>)
    )
))]
pub async fn search_metadata(
    State(state): State<AppState>,
    Query(query): Query<UnifiedSearchQuery>,
) -> AppResult<Json<Vec<SearchedMetadata>>> {
    let search_query = MetadataSearchQuery::new(&query.keyword);
    let results = state
        .metadata
        .search_provider(&search_query, query.source)
        .await?;
    Ok(Json(results))
}

/// Find best matching metadata from a specific data source
///
/// Returns a single best matching result based on keyword and optional year filter.
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/search/metadata/find",
    tag = "search",
    params(UnifiedSearchQuery),
    responses(
        (status = 200, description = "Best matching metadata", body = Option<SearchedMetadata>)
    )
))]
pub async fn find_metadata(
    State(state): State<AppState>,
    Query(query): Query<UnifiedSearchQuery>,
) -> AppResult<Json<Option<SearchedMetadata>>> {
    let mut search_query = MetadataSearchQuery::new(&query.keyword);
    if let Some(year) = query.year {
        search_query = search_query.with_year(year);
    }
    let result = state
        .metadata
        .find_provider(&search_query, query.source)
        .await?;
    Ok(Json(result))
}

/// Get metadata detail by external ID from a specific data source
///
/// Returns full metadata for a specific subject/show.
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/search/metadata/detail",
    tag = "search",
    params(DetailQuery),
    responses(
        (status = 200, description = "Metadata detail", body = Option<SearchedMetadata>)
    )
))]
pub async fn get_metadata_detail(
    State(state): State<AppState>,
    Query(query): Query<DetailQuery>,
) -> AppResult<Json<Option<SearchedMetadata>>> {
    let result = state
        .metadata
        .get_provider_detail(&query.external_id, query.source)
        .await?;
    Ok(Json(result))
}
