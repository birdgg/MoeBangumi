//! Shared value objects used across domain modules.
//!
//! These value objects are used by multiple domains and don't belong
//! to any single aggregate.

mod clearable;

pub use clearable::Clearable;
