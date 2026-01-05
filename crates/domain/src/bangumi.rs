//! Bangumi domain module.
//!
//! Contains the Bangumi entity, repository trait, and related types.

mod entity;
mod repository;
mod request;
mod source_type;

pub use entity::Bangumi;
pub use repository::BangumiRepository;
pub use request::{CreateBangumiError, CreateBangumiRequest};
pub use source_type::SourceType;
