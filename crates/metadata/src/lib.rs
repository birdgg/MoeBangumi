//! Metadata management module for moe-bangumi
//!
//! This crate provides:
//! - `MetadataService` - Query and CRUD operations for metadata (BGM.tv, TMDB)
//! - `PosterService` - Poster image downloading with deduplication
//! - `MetadataHandle` - Actor interface for async operations with internal scheduling
//!
//! # Example
//!
//! ```rust,ignore
//! use metadata::{create_metadata_actor, MetadataService, PosterService};
//!
//! // Create services
//! let metadata_service = Arc::new(MetadataService::new(db.clone(), bgmtv, tmdb));
//! let poster_service = Arc::new(PosterService::with_client_provider(provider, posters_path));
//!
//! // Create actor with internal scheduling
//! let metadata_handle = create_metadata_actor(
//!     db.clone(),
//!     metadata_service.clone(),
//!     poster_service,
//! );
//!
//! // Fire-and-forget poster download
//! metadata_handle.download(metadata_id, poster_url);
//!
//! // Manual sync trigger
//! metadata_handle.trigger_sync();
//! ```

mod actor;
mod error;
mod poster;
mod service;

// Public exports
pub use actor::{create_metadata_actor, create_metadata_actor_with_interval, MetadataHandle, SyncStats};
pub use error::{MetadataError, PosterError};
pub use poster::{ClientProvider, PosterService};
pub use service::{FetchedMetadata, MetadataService};

// Re-export from model crate for convenience
pub use model::{
    CreateMetadata, Metadata, MetadataRepository, MetadataToSync, Platform, UpdateMetadata,
};
