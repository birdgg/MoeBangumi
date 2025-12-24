use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use serde::Deserialize;
use utoipa::IntoParams;

use downloader::{DownloaderClient, DownloaderConfig, Downloader};

use crate::models::{Bangumi, CreateBangumi, CreateRss, Settings, UpdateSettings};
use crate::repositories::{BangumiRepository, CacheRepository, RssRepository};
use crate::state::AppState;
use tmdb::DiscoverBangumiParams;

// Cache TTL constants (in seconds)
const MIKAN_SEARCH_CACHE_TTL: i64 = 604800; // 1 week
const MIKAN_DETAIL_CACHE_TTL: i64 = 2592000; // 30 days

/// Query parameters for keyword search
#[derive(Debug, Deserialize, IntoParams)]
pub struct SearchQuery {
    /// Keyword to search
    pub keyword: String,
}

/// Query parameters for ID lookup
#[derive(Debug, Deserialize, IntoParams)]
pub struct IdQuery {
    /// ID to lookup
    pub id: String,
}

/// Request body for testing downloader connection
#[derive(Debug, Deserialize, utoipa::ToSchema)]
pub struct TestDownloaderRequest {
    /// Downloader type (e.g., "qbittorrent")
    #[serde(rename = "type")]
    pub downloader_type: downloader::DownloaderType,
    /// Downloader Web UI URL
    pub url: String,
    /// Username
    pub username: String,
    /// Password
    pub password: String,
}

/// Search for bangumi (Japanese anime) on BGM.tv
#[utoipa::path(
    get,
    path = "/api/search/bgmtv",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results", body = Vec<bgmtv::Subject>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_bgmtv(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> impl IntoResponse {
    match state.bgmtv.search_bangumi(&query.keyword).await {
        Ok(response) => (StatusCode::OK, Json(response.data)).into_response(),
        Err(e) => {
            tracing::error!("Failed to search bangumi: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Search for anime on TMDB using discover API
#[utoipa::path(
    get,
    path = "/api/search/tmdb",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results from TMDB", body = Vec<tmdb::models::TvShow>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_tmdb(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> impl IntoResponse {
    let params = DiscoverBangumiParams {
        with_text_query: Some(query.keyword),
    };
    match state.tmdb.discover_bangumi(params).await {
        Ok(response) => (StatusCode::OK, Json(response.results)).into_response(),
        Err(e) => {
            tracing::error!("Failed to search TMDB: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Create a new bangumi
#[utoipa::path(
    post,
    path = "/api/bangumi",
    tag = "bangumi",
    request_body = CreateBangumi,
    responses(
        (status = 201, description = "Bangumi created successfully", body = Bangumi),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn create_bangumi(
    State(state): State<AppState>,
    Json(payload): Json<CreateBangumi>,
) -> impl IntoResponse {
    // Extract RSS entries before creating bangumi
    let rss_entries = payload.rss_entries.clone();

    match BangumiRepository::create(&state.db, payload).await {
        Ok(bangumi) => {
            // Create RSS subscriptions for the new bangumi
            for entry in rss_entries {
                let create_rss = CreateRss {
                    bangumi_id: bangumi.id,
                    url: entry.url,
                    enabled: true,
                    exclude_filters: entry.filters,
                };

                if let Err(e) = RssRepository::create(&state.db, create_rss).await {
                    tracing::error!("Failed to create RSS subscription: {}", e);
                }
            }

            (StatusCode::CREATED, Json(bangumi)).into_response()
        }
        Err(e) => {
            tracing::error!("Failed to create bangumi: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

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
) -> impl IntoResponse {
    match state.bgmtv.get_episodes(subject_id).await {
        Ok(response) => (StatusCode::OK, Json(response.data)).into_response(),
        Err(e) => {
            tracing::error!("Failed to get episodes for subject {}: {}", subject_id, e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Search for bangumi on Mikan
#[utoipa::path(
    get,
    path = "/api/search/mikan",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results from Mikan", body = Vec<mikan::SearchResult>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_mikan(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> impl IntoResponse {
    let cache_key = format!("mikan:search:{}", query.keyword);

    // Try cache first
    if let Ok(Some(cached)) = CacheRepository::get::<Vec<mikan::SearchResult>>(
        &state.db,
        &cache_key,
        MIKAN_SEARCH_CACHE_TTL,
    )
    .await
    {
        return (StatusCode::OK, Json(cached)).into_response();
    }

    // Fetch from Mikan
    match state.mikan.search_bangumi(&query.keyword).await {
        Ok(results) => {
            // Cache the results
            if let Err(e) = CacheRepository::set(&state.db, &cache_key, &results).await {
                tracing::warn!("Failed to cache Mikan search results: {}", e);
            }
            (StatusCode::OK, Json(results)).into_response()
        }
        Err(e) => {
            tracing::error!("Failed to search Mikan: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Get all bangumi
#[utoipa::path(
    get,
    path = "/api/bangumi",
    tag = "bangumi",
    responses(
        (status = 200, description = "List of all bangumi", body = Vec<Bangumi>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_bangumi(State(state): State<AppState>) -> impl IntoResponse {
    match BangumiRepository::get_all(&state.db).await {
        Ok(bangumi_list) => (StatusCode::OK, Json(bangumi_list)).into_response(),
        Err(e) => {
            tracing::error!("Failed to get all bangumi: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

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
) -> impl IntoResponse {
    let cache_key = format!("mikan:detail:{}", query.id);

    // Try cache first
    if let Ok(Some(cached)) =
        CacheRepository::get::<mikan::BangumiDetail>(&state.db, &cache_key, MIKAN_DETAIL_CACHE_TTL)
            .await
    {
        return (StatusCode::OK, Json(cached)).into_response();
    }

    // Fetch from Mikan
    match state.mikan.get_bangumi_detail(&query.id).await {
        Ok(detail) => {
            // Cache the results
            if let Err(e) = CacheRepository::set(&state.db, &cache_key, &detail).await {
                tracing::warn!("Failed to cache Mikan detail: {}", e);
            }
            (StatusCode::OK, Json(detail)).into_response()
        }
        Err(e) => {
            tracing::error!("Failed to get Mikan RSS for {}: {}", query.id, e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Get application settings
#[utoipa::path(
    get,
    path = "/api/settings",
    tag = "settings",
    responses(
        (status = 200, description = "Application settings", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_settings(State(state): State<AppState>) -> impl IntoResponse {
    let settings = state.settings.get().await;
    (StatusCode::OK, Json(settings)).into_response()
}

/// Update application settings
#[utoipa::path(
    patch,
    path = "/api/settings",
    tag = "settings",
    request_body = UpdateSettings,
    responses(
        (status = 200, description = "Settings updated successfully", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn update_settings(
    State(state): State<AppState>,
    Json(payload): Json<UpdateSettings>,
) -> impl IntoResponse {
    match state.settings.update(payload).await {
        Ok(settings) => (StatusCode::OK, Json(settings)).into_response(),
        Err(e) => {
            tracing::error!("Failed to update settings: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Reset settings to defaults
#[utoipa::path(
    post,
    path = "/api/settings/reset",
    tag = "settings",
    responses(
        (status = 200, description = "Settings reset successfully", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn reset_settings(State(state): State<AppState>) -> impl IntoResponse {
    match state.settings.reset().await {
        Ok(settings) => (StatusCode::OK, Json(settings)).into_response(),
        Err(e) => {
            tracing::error!("Failed to reset settings: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Test downloader connection with provided credentials
#[utoipa::path(
    post,
    path = "/api/downloader/test",
    tag = "downloader",
    request_body = TestDownloaderRequest,
    responses(
        (status = 200, description = "Connection successful"),
        (status = 401, description = "Authentication failed"),
        (status = 500, description = "Connection error")
    )
)]
pub async fn test_downloader_connection(
    Json(payload): Json<TestDownloaderRequest>,
) -> impl IntoResponse {
    let config = DownloaderConfig {
        downloader_type: payload.downloader_type,
        url: payload.url,
        username: Some(payload.username),
        password: Some(payload.password),
    };

    let client = match DownloaderClient::from_config(config) {
        Ok(c) => c,
        Err(e) => {
            tracing::error!("Failed to create downloader client: {}", e);
            return (StatusCode::INTERNAL_SERVER_ERROR, "Failed to create downloader client")
                .into_response();
        }
    };

    match client.authenticate().await {
        Ok(()) => {
            tracing::info!("Downloader connection test successful");
            (StatusCode::OK, "Connection successful").into_response()
        }
        Err(e) => {
            tracing::error!("Downloader connection test failed: {}", e);
            (StatusCode::UNAUTHORIZED, format!("Connection failed: {}", e)).into_response()
        }
    }
}
