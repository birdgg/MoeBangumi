//! Infrastructure layer
//!
//! Contains:
//! - Configuration and environment handling
//! - Database connection management
//! - Error types
//! - Application state
//! - Utility libraries

pub mod banner;
pub mod config;
pub mod db;
pub mod error;
pub mod state;
pub mod utils;

pub use banner::print_banner;
pub use config::{default_data_path, Config, Environment};
pub use db::{create_pool, DatabaseError};
pub use error::{AppError, AppResult};
pub use state::AppState;
