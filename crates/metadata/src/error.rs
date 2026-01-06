//! Error types for metadata provider operations

use crate::MetadataSource;

/// Errors that can occur when using metadata providers
#[derive(Debug, thiserror::Error)]
pub enum ProviderError {
    #[error("Data source {0:?} is not available")]
    SourceNotAvailable(MetadataSource),

    #[error("BGM.tv error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),

    #[error("TMDB error: {0}")]
    Tmdb(#[from] tmdb::TmdbError),
}
