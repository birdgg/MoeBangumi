mod client;
mod episodes;
mod error;
pub mod models;
pub mod parser;
mod search;
mod subject;

pub use client::{BgmtvClient, ClientProvider};
pub use error::BgmtvError;
pub use models::{
    CalendarCollection, CalendarDay, CalendarRating, CalendarSubject, Episode, EpisodeType,
    EpisodesResponse, SearchFilter, SearchSubjectsRequest, SearchSubjectsResponse, Subject,
    SubjectDetail, SubjectImages, SubjectType, Weekday,
};
pub use parser::{parse_name, parse_subject, parse_subject_detail, NameParseResult, ParsedSubject};

pub type Result<T> = std::result::Result<T, BgmtvError>;
