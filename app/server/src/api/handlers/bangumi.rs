use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};

use crate::error::AppResult;
use crate::models::{Bangumi, BangumiWithRss, CreateBangumi, UpdateBangumiRequest};
use crate::state::AppState;

/// Create a new bangumi
#[cfg_attr(feature = "openapi", utoipa::path(
    post,
    path = "/api/bangumi",
    tag = "bangumi",
    request_body = CreateBangumi,
    responses(
        (status = 201, description = "Bangumi created successfully", body = Bangumi)
    )
))]
pub async fn create_bangumi(
    State(state): State<AppState>,
    Json(payload): Json<CreateBangumi>,
) -> AppResult<(StatusCode, Json<Bangumi>)> {
    let bangumi = state.services.bangumi.create(payload).await?;
    Ok((StatusCode::CREATED, Json(bangumi)))
}

/// Get all bangumi
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/bangumi",
    tag = "bangumi",
    responses(
        (status = 200, description = "List of all bangumi", body = Vec<Bangumi>)
    )
))]
pub async fn get_bangumi(State(state): State<AppState>) -> AppResult<Json<Vec<Bangumi>>> {
    let bangumi_list = state.services.bangumi.get_all().await?;
    Ok(Json(bangumi_list))
}

/// Get a bangumi by ID with its RSS subscriptions
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/bangumi/{id}",
    tag = "bangumi",
    params(
        ("id" = i64, Path, description = "Bangumi ID")
    ),
    responses(
        (status = 200, description = "Bangumi with RSS subscriptions", body = BangumiWithRss),
        (status = 404, description = "Bangumi not found")
    )
))]
pub async fn get_bangumi_by_id(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> AppResult<Json<BangumiWithRss>> {
    let bangumi_with_rss = state.services.bangumi.get_with_rss(id).await?;
    Ok(Json(bangumi_with_rss))
}

/// Update a bangumi
#[cfg_attr(feature = "openapi", utoipa::path(
    patch,
    path = "/api/bangumi/{id}",
    tag = "bangumi",
    params(
        ("id" = i64, Path, description = "Bangumi ID")
    ),
    request_body = UpdateBangumiRequest,
    responses(
        (status = 200, description = "Bangumi updated successfully", body = BangumiWithRss),
        (status = 404, description = "Bangumi not found")
    )
))]
pub async fn update_bangumi(
    State(state): State<AppState>,
    Path(id): Path<i64>,
    Json(payload): Json<UpdateBangumiRequest>,
) -> AppResult<Json<BangumiWithRss>> {
    let bangumi_with_rss = state.services.bangumi.update(id, payload).await?;
    Ok(Json(bangumi_with_rss))
}
