//! RSS feed fetching and parsing library
//!
//! Provides utilities for fetching and parsing RSS feeds from various sources.

mod client;
mod error;
pub mod models;
mod parsers;

pub use client::{ClientProvider, RssClient};
pub use error::RssError;
pub use models::{FetchContext, FetchResult, RssItem, RssSource};

pub type Result<T> = std::result::Result<T, RssError>;
