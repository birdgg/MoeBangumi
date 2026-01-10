mod client;
mod discover;
mod error;
pub mod models;
mod movie;
mod search;
mod tv;

pub use client::{ApiKey, TmdbClient};
pub use discover::DiscoverBangumiParams;
pub use error::TmdbError;

pub type Result<T> = std::result::Result<T, TmdbError>;
