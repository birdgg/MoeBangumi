use axum::{
    extract::{Query, State},
    Json,
};

use crate::error::AppResult;
use crate::repositories::CacheRepository;
use crate::state::AppState;

use super::{IdQuery, MIKAN_DETAIL_CACHE_TTL};

/// Get bangumi detail with RSS URLs from Mikan
#[utoipa::path(
    get,
    path = "/api/mikan/rss",
    tag = "mikan",
    params(IdQuery),
    responses(
        (status = 200, description = "Bangumi detail with subgroups and RSS URLs", body = mikan::BangumiDetail),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_mikan_rss(
    State(state): State<AppState>,
    Query(query): Query<IdQuery>,
) -> AppResult<Json<mikan::BangumiDetail>> {
    let cache_key = format!("mikan:detail:{}", query.id);

    // Try cache first
    if let Ok(Some(cached)) =
        CacheRepository::get::<mikan::BangumiDetail>(&state.db, &cache_key, MIKAN_DETAIL_CACHE_TTL)
            .await
    {
        return Ok(Json(cached));
    }

    // Fetch from Mikan
    let detail = state.mikan.get_bangumi_detail(&query.id).await?;

    // Cache the results
    if let Err(e) = CacheRepository::set(&state.db, &cache_key, &detail).await {
        tracing::warn!("Failed to cache Mikan detail: {}", e);
    }

    Ok(Json(detail))
}
