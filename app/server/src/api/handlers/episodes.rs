use axum::{
    extract::{Path, State},
    Json,
};
use metadata::{Episode, MetadataSource};

use crate::error::AppResult;
use crate::state::AppState;

pub async fn get_episodes(
    State(state): State<AppState>,
    Path(subject_id): Path<i64>,
) -> AppResult<Json<Vec<Episode>>> {
    let episodes = state
        .services
        .metadata_client
        .get_episodes(&subject_id.to_string(), MetadataSource::Bgmtv)
        .await?;
    Ok(Json(episodes))
}
