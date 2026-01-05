use thiserror::Error;

#[derive(Debug, Error)]
pub enum PathGenError {
    #[error("Invalid path component: {0}")]
    InvalidComponent(String),

    #[error("Missing required field: {0}")]
    MissingField(String),
}

pub type Result<T> = std::result::Result<T, PathGenError>;
