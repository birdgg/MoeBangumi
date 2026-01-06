//! Metadata provider adapters for different data sources

mod bgmtv_adapter;
mod tmdb_adapter;

pub use bgmtv_adapter::BgmtvProvider;
pub use tmdb_adapter::TmdbProvider;
