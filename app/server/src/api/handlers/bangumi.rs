use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};

use crate::error::AppResult;
use crate::models::{Bangumi, BangumiWithRss, CreateBangumi, UpdateBangumiRequest};
use crate::state::AppState;

pub async fn create_bangumi(
    State(state): State<AppState>,
    Json(payload): Json<CreateBangumi>,
) -> AppResult<(StatusCode, Json<Bangumi>)> {
    let bangumi = state.services.bangumi.create(payload).await?;
    Ok((StatusCode::CREATED, Json(bangumi)))
}

pub async fn get_bangumi(State(state): State<AppState>) -> AppResult<Json<Vec<Bangumi>>> {
    let bangumi_list = state.services.bangumi.get_all().await?;
    Ok(Json(bangumi_list))
}

pub async fn get_bangumi_by_id(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> AppResult<Json<BangumiWithRss>> {
    let bangumi_with_rss = state.services.bangumi.get_with_rss(id).await?;
    Ok(Json(bangumi_with_rss))
}

pub async fn update_bangumi(
    State(state): State<AppState>,
    Path(id): Path<i64>,
    Json(payload): Json<UpdateBangumiRequest>,
) -> AppResult<Json<BangumiWithRss>> {
    let bangumi_with_rss = state.services.bangumi.update(id, payload).await?;
    Ok(Json(bangumi_with_rss))
}
