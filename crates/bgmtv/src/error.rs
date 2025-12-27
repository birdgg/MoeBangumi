use thiserror::Error;

#[derive(Debug, Error)]
pub enum BgmtvError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("Failed to parse JSON response: {0}")]
    Json(#[from] serde_json::Error),

    #[error("API error: {status_code} - {message}")]
    Api { status_code: u16, message: String },

    #[error("HTTP client error: {0}")]
    HttpClient(String),
}
