use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde::Serialize;
use thiserror::Error;

/// 统一的应用错误类型
#[derive(Debug, Error)]
pub enum AppError {
    /// 资源未找到
    #[error("{0}")]
    NotFound(String),

    /// 请求参数无效
    #[error("{0}")]
    BadRequest(String),

    /// 数据库错误
    #[error("数据库错误: {0}")]
    Database(#[from] sqlx::Error),

    /// 设置服务错误
    #[error("设置错误: {0}")]
    Settings(#[from] crate::services::SettingsError),

    /// 下载器错误
    #[error("下载器错误: {0}")]
    Downloader(#[from] downloader::DownloaderError),

    /// 日志服务错误
    #[error("日志错误: {0}")]
    Log(#[from] crate::services::LogError),

    /// 海报服务错误
    #[error("海报错误: {0}")]
    Poster(#[from] crate::metadata_service::PosterError),

    /// Bangumi 服务错误
    #[error("Bangumi 错误: {0}")]
    Bangumi(#[from] crate::services::BangumiError),

    /// 外部 API 错误 (bgmtv, tmdb, mikan)
    #[error("外部 API 错误: {0}")]
    ExternalApi(String),

    /// 内部错误
    #[error("内部错误: {0}")]
    Internal(String),
}

/// API 错误响应体
#[derive(Debug, Serialize)]
struct ErrorResponse {
    error: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    details: Option<String>,
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let (status, error_message, details) = match &self {
            AppError::NotFound(msg) => (StatusCode::NOT_FOUND, msg.clone(), None),
            AppError::BadRequest(msg) => (StatusCode::BAD_REQUEST, msg.clone(), None),
            AppError::Database(e) => {
                tracing::error!("Database error: {}", e);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "数据库错误".to_string(),
                    Some(e.to_string()),
                )
            }
            AppError::Settings(e) => {
                tracing::error!("Settings error: {}", e);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "设置错误".to_string(),
                    Some(e.to_string()),
                )
            }
            AppError::Downloader(e) => {
                tracing::error!("Downloader error: {}", e);
                // 根据具体错误类型返回不同状态码
                let status = match e {
                    downloader::DownloaderError::NotConfigured => StatusCode::BAD_REQUEST,
                    downloader::DownloaderError::Auth(_) => StatusCode::UNAUTHORIZED,
                    _ => StatusCode::INTERNAL_SERVER_ERROR,
                };
                (status, "下载器错误".to_string(), Some(e.to_string()))
            }
            AppError::Log(e) => {
                tracing::error!("Log error: {}", e);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "日志服务错误".to_string(),
                    Some(e.to_string()),
                )
            }
            AppError::Poster(e) => {
                tracing::error!("Poster error: {}", e);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "海报服务错误".to_string(),
                    Some(e.to_string()),
                )
            }
            AppError::Bangumi(e) => {
                let status = match e {
                    crate::services::BangumiError::NotFound => StatusCode::NOT_FOUND,
                    _ => StatusCode::INTERNAL_SERVER_ERROR,
                };
                (status, "Bangumi 错误".to_string(), Some(e.to_string()))
            }
            AppError::ExternalApi(msg) => {
                tracing::error!("External API error: {}", msg);
                (
                    StatusCode::BAD_GATEWAY,
                    "外部 API 错误".to_string(),
                    Some(msg.clone()),
                )
            }
            AppError::Internal(msg) => {
                tracing::error!("Internal error: {}", msg);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "内部错误".to_string(),
                    Some(msg.clone()),
                )
            }
        };

        let body = ErrorResponse {
            error: error_message,
            details,
        };

        (status, Json(body)).into_response()
    }
}

/// 便捷类型别名
pub type AppResult<T> = Result<T, AppError>;

// 便捷方法
impl AppError {
    /// 创建 NotFound 错误
    pub fn not_found(msg: impl Into<String>) -> Self {
        Self::NotFound(msg.into())
    }

    /// 创建 BadRequest 错误
    pub fn bad_request(msg: impl Into<String>) -> Self {
        Self::BadRequest(msg.into())
    }

    /// 创建外部 API 错误
    pub fn external_api(msg: impl Into<String>) -> Self {
        Self::ExternalApi(msg.into())
    }

    /// 创建内部错误
    pub fn internal(msg: impl Into<String>) -> Self {
        Self::Internal(msg.into())
    }
}

// 为常见的外部错误实现 From
impl From<bgmtv::BgmtvError> for AppError {
    fn from(e: bgmtv::BgmtvError) -> Self {
        AppError::ExternalApi(e.to_string())
    }
}

impl From<mikan::MikanError> for AppError {
    fn from(e: mikan::MikanError) -> Self {
        AppError::ExternalApi(e.to_string())
    }
}

impl From<tmdb::TmdbError> for AppError {
    fn from(e: tmdb::TmdbError) -> Self {
        AppError::ExternalApi(e.to_string())
    }
}

impl From<crate::services::CalendarError> for AppError {
    fn from(e: crate::services::CalendarError) -> Self {
        match e {
            crate::services::CalendarError::Database(e) => AppError::Database(e),
            crate::services::CalendarError::Provider(e) => AppError::ExternalApi(e.to_string()),
            crate::services::CalendarError::Mikan(e) => AppError::ExternalApi(e.to_string()),
            crate::services::CalendarError::JsonParse(e) => {
                AppError::internal(format!("JSON parse error: {}", e))
            }
            crate::services::CalendarError::Http(e) => AppError::ExternalApi(e.to_string()),
        }
    }
}

impl From<crate::metadata_service::MetadataError> for AppError {
    fn from(e: crate::metadata_service::MetadataError) -> Self {
        use crate::metadata_service::MetadataError;
        match e {
            MetadataError::Database(e) => AppError::Database(e),
            MetadataError::NotFound(id) => {
                AppError::NotFound(format!("Metadata not found: id={}", id))
            }
            MetadataError::Provider(e) => AppError::ExternalApi(e.to_string()),
        }
    }
}

impl From<metadata::ProviderError> for AppError {
    fn from(e: metadata::ProviderError) -> Self {
        use metadata::ProviderError;
        match e {
            ProviderError::Bgmtv(e) => AppError::ExternalApi(e.to_string()),
            ProviderError::Tmdb(e) => AppError::ExternalApi(e.to_string()),
            ProviderError::SourceNotAvailable(source) => {
                AppError::BadRequest(format!("Data source {:?} is not available", source))
            }
        }
    }
}
