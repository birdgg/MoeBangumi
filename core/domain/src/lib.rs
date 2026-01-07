//! Domain layer - Core business logic
//!
//! This module contains:
//! - `config`: Application configuration
//! - `models`: Domain entities and data structures
//! - `repositories`: Data access layer
//! - `services`: Business logic and actors
//! - `utils`: Shared utilities (pathgen, rss, tracing)

pub mod config;
pub mod models;
pub mod repositories;
pub mod services;
pub mod utils;

// Re-export commonly used types
pub use config::*;
pub use models::*;
pub use repositories::*;
pub use services::*;
