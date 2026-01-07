use axum::{
    extract::{Path, State},
    Json,
};
use metadata::Episode;

use crate::error::AppResult;
use crate::state::AppState;

/// Get episodes by subject ID from BGM.tv
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/episodes/{subject_id}",
    tag = "episodes",
    params(
        ("subject_id" = i64, Path, description = "BGM.tv subject ID")
    ),
    responses(
        (status = 200, description = "Episodes list", body = Vec<Episode>)
    )
))]
pub async fn get_episodes(
    State(state): State<AppState>,
    Path(subject_id): Path<i64>,
) -> AppResult<Json<Vec<Episode>>> {
    let episodes = state.metadata.get_episodes(subject_id).await?;
    Ok(Json(episodes))
}
