use thiserror::Error;

#[derive(Debug, Error)]
pub enum RssError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("Failed to parse RSS feed: {0}")]
    Parse(String),

    #[error("HTTP client error: {0}")]
    HttpClient(String),
}
