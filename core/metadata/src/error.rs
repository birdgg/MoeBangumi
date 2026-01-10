//! Error types for metadata operations

/// Errors that can occur when using metadata client
#[derive(Debug, thiserror::Error)]
pub enum MetadataError {
    #[error("BGM.tv error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),

    #[error("TMDB error: {0}")]
    Tmdb(#[from] tmdb::TmdbError),
}
