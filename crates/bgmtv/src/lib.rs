mod client;
mod error;
pub mod models;
mod search;

pub use client::BgmtvClient;
pub use error::BgmtvError;
pub use models::{SearchFilter, SearchSubjectsRequest, SearchSubjectsResponse, SubjectType};

pub type Result<T> = std::result::Result<T, BgmtvError>;
