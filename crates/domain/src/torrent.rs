//! Torrent domain module.
//!
//! Contains the Torrent entity, repository trait, and related types.

mod entity;
mod priority;
mod repository;
mod request;

pub use entity::{ComparableTorrent, SubtitleType, Torrent};
pub use priority::{PriorityCalculator, PriorityConfig, PriorityScore, SubtitleLanguageSet};
pub use repository::TorrentRepository;
pub use request::{CreateTorrentError, CreateTorrentRequest};
