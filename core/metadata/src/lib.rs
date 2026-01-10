//! Unified metadata client for TMDB and BGM.tv
//!
//! This crate provides:
//! 1. A new unified `MetadataClient` with `get_tv_detail`, `get_season_detail`,
//!    `get_movie_detail`, and `search` methods
//! 2. Compatibility layer for existing code (MetadataProvider trait, BgmtvProvider, TmdbProvider)
//!
//! # New API Example
//!
//! ```ignore
//! use metadata::MetadataClient;
//! use std::sync::Arc;
//!
//! let client = MetadataClient::new(tmdb_client, bgmtv_client);
//!
//! // Search (TMDB first, fallback to BGM.tv)
//! let results = client.search("葬送のフリーレン").await?;
//!
//! // Get TV details
//! let tv = client.get_tv_detail(12345).await?;
//!
//! // Get season details
//! let season = client.get_season_detail(12345, 1).await?;
//!
//! // Get movie details
//! let movie = client.get_movie_detail(67890).await?;
//! ```

mod client;
mod compat;
mod error;
pub mod models;

// New unified client
pub use client::MetadataClient;
pub use error::MetadataError;
pub use models::{
    EpisodeInfo, MediaType, MovieDetail, SearchResult, SeasonDetail, SeasonSummary, TvDetail,
};

// Compatibility exports for existing code
pub use compat::{
    BgmtvProvider, Episode, EpisodeType, MetadataProvider, MetadataSource, Platform,
    ProviderError, SearchQuery, SearchedMetadata, TmdbProvider,
};
