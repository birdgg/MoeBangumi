mod calendar;
mod client;
mod episodes;
mod error;
pub mod models;
mod search;
mod subject;

pub use client::{BgmtvClient, ClientProvider};
pub use error::BgmtvError;
pub use models::{
    CalendarDay, CalendarSubject, Episode, EpisodeType, EpisodesResponse, SearchFilter,
    SearchSubjectsRequest, SearchSubjectsResponse, Subject, SubjectDetail, SubjectType, Weekday,
};

pub type Result<T> = std::result::Result<T, BgmtvError>;
