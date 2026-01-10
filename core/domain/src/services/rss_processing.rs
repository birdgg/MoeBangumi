//! RSS processing module.
//!
//! This module handles RSS feed processing including fetching, filtering,
//! parsing, and scheduling downloads.

mod adapters;
mod filters;
mod processor;
mod service;
mod traits;

pub use service::RssProcessingService;
