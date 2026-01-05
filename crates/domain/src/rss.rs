//! RSS domain module.
//!
//! Contains the RSS subscription entity, repository trait, and related types.

mod entity;
mod repository;
mod request;

pub use entity::{format_rss_title, Rss};
pub use repository::RssRepository;
pub use request::{CreateRssError, CreateRssRequest};
