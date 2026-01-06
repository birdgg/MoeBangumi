//! Self-update service for moe-bangumi
//!
//! This crate provides functionality for:
//! - Checking for new versions from GitHub Releases
//! - Downloading and installing updates
//! - Background version checking with configurable intervals
//!
//! # Docker Support
//!
//! In Docker environments, the binary is downloaded to a mounted volume (`/data/bin/`)
//! rather than replacing the running executable. A restart is required to use the new version.

mod config;
mod error;
mod models;
mod service;

pub use config::UpdateConfig;
pub use error::UpdateError;
pub use models::{ReleaseInfo, UpdateStatus, VersionInfo};
pub use service::{UpdateService, UpdateServiceHandle};
