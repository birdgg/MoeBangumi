//! Infrastructure layer
//!
//! Contains:
//! - Database connection management
//! - Error types
//! - Application state

pub mod banner;
pub mod db;
pub mod error;
pub mod state;

pub use banner::print_banner;
pub use domain::{default_data_path, Config, Environment};
pub use db::{create_pool, DatabaseError};
pub use error::{AppError, AppResult};
pub use state::AppState;
