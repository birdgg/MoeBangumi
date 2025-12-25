use axum::{
    extract::{Query, State},
    Json,
};

use crate::error::AppResult;
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
    let mikan = state.mikan.clone();
    let id = query.id.clone();

    let detail = state
        .cache
        .get_or_fetch(&cache_key, MIKAN_DETAIL_CACHE_TTL, || async move {
            mikan.get_bangumi_detail(&id).await
        })
        .await?;

    Ok(Json(detail))
}
