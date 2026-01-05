//! Metadata domain module.
//!
//! Contains the Metadata entity, repository trait, and related types.

mod entity;
mod episode_offset;
mod platform;
mod repository;
mod request;
mod season;

pub use entity::Metadata;
pub use episode_offset::EpisodeOffset;
pub use platform::Platform;
pub use repository::MetadataRepository;
pub use request::{CreateMetadataError, CreateMetadataRequest};
pub use season::{Season, SeasonError};
