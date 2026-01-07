use axum::{
    extract::{Path, State},
    Json,
};

use crate::error::AppResult;
use crate::models::{Metadata, UpdateMetadata};
use crate::state::AppState;

/// Get all metadata
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/metadata",
    tag = "metadata",
    responses(
        (status = 200, description = "List of all metadata", body = Vec<Metadata>)
    )
))]
pub async fn get_metadata(State(state): State<AppState>) -> AppResult<Json<Vec<Metadata>>> {
    let metadata_list = state.metadata.get_all().await?;
    Ok(Json(metadata_list))
}

/// Get a metadata by ID
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/metadata/{id}",
    tag = "metadata",
    params(
        ("id" = i64, Path, description = "Metadata ID")
    ),
    responses(
        (status = 200, description = "Metadata details", body = Metadata),
        (status = 404, description = "Metadata not found")
    )
))]
pub async fn get_metadata_by_id(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> AppResult<Json<Metadata>> {
    let metadata = state.metadata.get_by_id(id).await?;
    Ok(Json(metadata))
}

/// Update a metadata
#[cfg_attr(feature = "openapi", utoipa::path(
    patch,
    path = "/api/metadata/{id}",
    tag = "metadata",
    params(
        ("id" = i64, Path, description = "Metadata ID")
    ),
    request_body = UpdateMetadata,
    responses(
        (status = 200, description = "Metadata updated successfully", body = Metadata),
        (status = 404, description = "Metadata not found")
    )
))]
pub async fn update_metadata(
    State(state): State<AppState>,
    Path(id): Path<i64>,
    Json(payload): Json<UpdateMetadata>,
) -> AppResult<Json<Metadata>> {
    let metadata = state.metadata.update(id, payload).await?;
    Ok(Json(metadata))
}
