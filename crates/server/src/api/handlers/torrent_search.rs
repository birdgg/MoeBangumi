use axum::{
    extract::{Query, State},
    Json,
};

use crate::error::AppResult;
use crate::models::TorrentSearchResult;
use crate::state::AppState;

use super::{TorrentSearchQuery, TORRENT_SEARCH_CACHE_TTL};

/// Search for torrents from a specific source (Nyaa or Mikan)
#[utoipa::path(
    get,
    path = "/api/torrents/search",
    tag = "search",
    params(TorrentSearchQuery),
    responses(
        (status = 200, description = "Search results from the specified source", body = Vec<TorrentSearchResult>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_torrents(
    State(state): State<AppState>,
    Query(query): Query<TorrentSearchQuery>,
) -> AppResult<Json<Vec<TorrentSearchResult>>> {
    let cache_key = format!("torrent:search:{}:{}", query.source, query.keyword);
    let torrent_search = state.torrent_search.clone();
    let keyword = query.keyword.clone();
    let source = query.source;

    let results = state
        .cache
        .get_or_fetch(&cache_key, TORRENT_SEARCH_CACHE_TTL, || async move {
            torrent_search.search(&keyword, source).await
        })
        .await?;

    Ok(Json(results))
}
