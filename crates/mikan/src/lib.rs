mod client;
mod error;
pub mod models;

pub use client::{ClientProvider, MikanClient};
pub use error::MikanError;
pub use models::{BangumiDetail, Episode, Season, SeasonalBangumi, SearchResult, Subgroup};

pub type Result<T> = std::result::Result<T, MikanError>;
