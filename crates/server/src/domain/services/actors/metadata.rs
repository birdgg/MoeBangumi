//! Metadata management module
//!
//! This module provides:
//! - `MetadataService` - Query and CRUD operations for metadata (BGM.tv, TMDB)
//! - `PosterService` - Poster image downloading with deduplication
//! - `MetadataHandle` - Actor interface for async operations with internal scheduling

mod actor;
mod error;
mod poster;
mod service;

// Public exports
pub use actor::{create_metadata_actor, create_metadata_actor_with_interval, MetadataHandle, SyncStats};
pub use error::{MetadataError, PosterError};
pub use poster::{ClientProvider, PosterService};
pub use service::{FetchedMetadata, MetadataService};
