//! Unified metadata provider abstraction layer
//!
//! This crate provides a standardized interface for searching metadata
//! from different data sources (BGM.tv, TMDB).
//!
//! See [README.md](../README.md) for detailed documentation.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────┐
//! │           MetadataProvider trait        │
//! │  search(&SearchQuery) -> Vec<Searched>  │
//! │  find(&SearchQuery) -> Option<Searched> │
//! └─────────────────────────────────────────┘
//!              △                    △
//!              │                    │
//!    ┌─────────┴──────┐   ┌─────────┴──────┐
//!    │  BgmtvProvider │   │  TmdbProvider  │
//!    └────────────────┘   └────────────────┘
//! ```
//!
//! # Example
//!
//! ```ignore
//! use metadata::{MetadataProvider, SearchQuery, BgmtvProvider};
//! use std::sync::Arc;
//!
//! let provider = BgmtvProvider::new(Arc::new(client));
//! let query = SearchQuery::new("葬送のフリーレン");
//! let results = provider.search(&query).await?;
//! ```

mod adapters;
mod error;
mod models;
mod parsed_subject;
mod provider;

pub use adapters::{BgmtvProvider, TmdbProvider};
pub use error::ProviderError;
pub use models::{Episode, EpisodeType, MetadataSource, Platform, SearchQuery, SearchedMetadata};
pub use parsed_subject::{parse_subject, parse_subject_detail, ParsedSubject};
pub use provider::MetadataProvider;
