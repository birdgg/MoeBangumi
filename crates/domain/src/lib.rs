//! Domain layer for moe-bangumi.
//!
//! This crate contains the core business logic and domain model for the
//! moe-bangumi application. It is designed to be independent of external
//! concerns like databases, web frameworks, or external APIs.
//!
//! # Architecture
//!
//! The domain layer follows Clean Architecture and DDD principles:
//!
//! - **Domain Modules**: Each aggregate has its own module (bangumi, metadata, rss, torrent)
//! - **Shared**: Common value objects used across domains
//!
//! # Module Structure
//!
//! Each domain module contains:
//! - **Entity**: Core business object with identity
//! - **Repository**: Abstract interface for data persistence (trait only)
//! - **Request**: Validated creation request with specific error types
//!
//! # Dependencies
//!
//! This crate has minimal dependencies and should not depend on any
//! infrastructure-specific crates (database, HTTP, etc.).

pub mod bangumi;
pub mod error;
pub mod metadata;
pub mod rss;
pub mod shared;
pub mod torrent;

// Re-exports for convenience
pub use bangumi::{Bangumi, BangumiRepository, CreateBangumiError, CreateBangumiRequest, SourceType};
pub use error::{DomainError, DomainResult};
pub use metadata::{CreateMetadataError, CreateMetadataRequest, EpisodeOffset, Metadata, MetadataRepository, Platform, Season, SeasonError};
pub use rss::{format_rss_title, CreateRssError, CreateRssRequest, Rss, RssRepository};
pub use shared::*;
pub use torrent::{
    ComparableTorrent, CreateTorrentError, CreateTorrentRequest, PriorityCalculator,
    PriorityConfig, PriorityScore, SubtitleLanguageSet, SubtitleType, Torrent, TorrentRepository,
};
