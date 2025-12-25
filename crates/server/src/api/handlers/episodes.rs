use axum::{
    extract::{Path, State},
    Json,
};

use crate::error::AppResult;
use crate::state::AppState;

/// Get episodes by subject ID from BGM.tv
#[utoipa::path(
    get,
    path = "/api/episodes/{subject_id}",
    tag = "episodes",
    params(
        ("subject_id" = i64, Path, description = "BGM.tv subject ID")
    ),
    responses(
        (status = 200, description = "Episodes list", body = Vec<bgmtv::Episode>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_episodes(
    State(state): State<AppState>,
    Path(subject_id): Path<i64>,
) -> AppResult<Json<Vec<bgmtv::Episode>>> {
    let response = state.bgmtv.get_episodes(subject_id).await?;
    Ok(Json(response.data))
}
