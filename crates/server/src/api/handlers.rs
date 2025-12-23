use axum::{
    extract::{Query, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use serde::Deserialize;
use utoipa::IntoParams;

use crate::state::AppState;

/// Query parameters for bangumi search
#[derive(Debug, Deserialize, IntoParams)]
pub struct SearchBangumiQuery {
    /// Keyword to search for bangumi
    pub bangumi: String,
}

/// Search for bangumi (Japanese anime) on BGM.tv
#[utoipa::path(
    get,
    path = "/api/search",
    tag = "search",
    params(SearchBangumiQuery),
    responses(
        (status = 200, description = "Search results", body = bgmtv::SearchSubjectsResponse),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_bangumi(
    State(state): State<AppState>,
    Query(query): Query<SearchBangumiQuery>,
) -> impl IntoResponse {
    match state.bgmtv.search_bangumi(&query.bangumi).await {
        Ok(response) => (StatusCode::OK, Json(response)).into_response(),
        Err(e) => {
            tracing::error!("Failed to search bangumi: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
