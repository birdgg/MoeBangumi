#[derive(Debug, thiserror::Error)]
pub enum MikanError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("HTTP request failed (with retry): {0}")]
    RequestMiddleware(#[from] reqwest_middleware::Error),

    #[error("Failed to parse HTML: {0}")]
    Parse(String),

    #[error("HTTP client error: {0}")]
    HttpClient(String),
}
