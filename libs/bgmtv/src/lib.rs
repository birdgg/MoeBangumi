mod client;
mod episodes;
mod error;
pub mod models;
mod search;
mod subject;

pub use client::BgmtvClient;
pub use error::BgmtvError;
pub use models::{
    CalendarCollection, CalendarDay, CalendarRating, CalendarSubject, Episode, EpisodeType,
    EpisodesResponse, SearchFilter, SearchSubjectsRequest, SearchSubjectsResponse, Subject,
    SubjectDetail, SubjectImages, SubjectType, Weekday,
};

pub type Result<T> = std::result::Result<T, BgmtvError>;
