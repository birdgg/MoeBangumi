mod client;
mod error;
pub mod models;
mod parsers;

pub use client::{ClientProvider, RssClient};
pub use error::RssError;
pub use models::{RssItem, RssSource};

pub type Result<T> = std::result::Result<T, RssError>;
