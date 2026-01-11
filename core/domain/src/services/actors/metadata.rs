//! Metadata management module
//!
//! This module provides:
//! - `PosterService` - Poster image downloading with deduplication
//! - `MetadataHandle` - Actor interface for async poster operations with internal scheduling

mod actor;
mod error;
mod poster;

// Public exports
pub use actor::{create_metadata_actor, create_metadata_actor_with_interval, MetadataHandle, SyncStats};
pub use error::{MetadataError, PosterError};
pub use poster::{ClientProvider, PosterService};
