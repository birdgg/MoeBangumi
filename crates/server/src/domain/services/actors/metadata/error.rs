use thiserror::Error;

/// Errors that can occur in the metadata module
#[derive(Debug, Error)]
pub enum MetadataError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Metadata not found: id={0}")]
    NotFound(i64),

    #[error("BGM.tv API error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),

    #[error("TMDB API error: {0}")]
    Tmdb(#[from] tmdb::TmdbError),
}

/// Errors that can occur when downloading or managing posters.
#[derive(Debug, Error)]
pub enum PosterError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("{operation} '{path}': {source}")]
    Io {
        operation: &'static str,
        path: String,
        source: std::io::Error,
    },

    #[error("Invalid poster path: {0}")]
    InvalidPath(String),

    #[error("HTTP error: {0}")]
    HttpStatus(u16),

    #[error("HTTP client error: {0}")]
    HttpClient(String),
}
