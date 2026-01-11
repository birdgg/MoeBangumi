mod client;
mod discover;
mod error;
mod movie;
mod search;
mod tv;
pub mod models;

pub use client::{ApiKey, TmdbClient};
pub use discover::DiscoverBangumiParams;
pub use error::TmdbError;
pub use models::{
    Genre, MediaType, MovieDetail, PaginatedResponse, SearchResultItem, Season, SeasonDetail,
    TvEpisode, TvSeriesDetail, TvShow,
};
pub use search::SearchMultiParams;

pub type Result<T> = std::result::Result<T, TmdbError>;
