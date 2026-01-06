mod client;
mod discover;
mod error;
pub mod models;

pub use client::{ApiKey, ClientProvider, TmdbClient};
pub use discover::DiscoverBangumiParams;
pub use error::TmdbError;

pub type Result<T> = std::result::Result<T, TmdbError>;
