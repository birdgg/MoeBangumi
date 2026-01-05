//! Domain layer - Core business logic
//!
//! This module contains:
//! - `models`: Domain entities and data structures
//! - `repositories`: Data access layer
//! - `services`: Business logic and actors

pub mod models;
pub mod repositories;
pub mod services;

// Re-export commonly used types
pub use models::*;
pub use repositories::*;
pub use services::*;
