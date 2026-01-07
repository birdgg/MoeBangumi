use axum::{
    extract::{Query, State},
    Json,
};
use parser::Parser;

use crate::error::AppResult;
use crate::state::AppState;

use super::{IdQuery, MIKAN_DETAIL_CACHE_TTL};

/// Get bangumi detail with RSS URLs from Mikan
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/mikan/rss",
    tag = "mikan",
    params(IdQuery),
    responses(
        (status = 200, description = "Bangumi detail with subgroups and RSS URLs", body = mikan::BangumiDetail)
    )
))]
pub async fn get_mikan_rss(
    State(state): State<AppState>,
    Query(query): Query<IdQuery>,
) -> AppResult<Json<mikan::BangumiDetail>> {
    let cache_key = format!("mikan:detail:{}", query.id);
    let mikan = state.mikan.clone();
    let id = query.id.clone();

    let mut detail = state
        .cache
        .get_or_fetch(&cache_key, MIKAN_DETAIL_CACHE_TTL, || async move {
            mikan.get_bangumi_detail(&id).await
        })
        .await?;

    // Parse episode metadata
    let parser = Parser::new();
    for subgroup in &mut detail.subgroups {
        for episode in &mut subgroup.episodes {
            if let Ok(parsed) = parser.parse(&episode.name) {
                episode.subtitle_languages = parsed.subtitle_language;
                episode.resolution = parsed.resolution;
            }
        }
    }

    Ok(Json(detail))
}
