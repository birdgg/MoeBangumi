mod client;
mod episodes;
mod error;
pub mod models;
mod search;

pub use client::{BgmtvClient, ClientProvider};
pub use error::BgmtvError;
pub use models::{
    Episode, EpisodeType, EpisodesResponse, SearchFilter, SearchSubjectsRequest,
    SearchSubjectsResponse, Subject, SubjectType,
};

pub type Result<T> = std::result::Result<T, BgmtvError>;
