//! Error types for the updater crate

use thiserror::Error;

/// Errors that can occur during update operations
#[derive(Debug, Error)]
pub enum UpdateError {
    /// Failed to fetch release information from GitHub
    #[error("Failed to fetch releases: {0}")]
    FetchError(String),

    /// Failed to parse version string
    #[error("Invalid version format: {0}")]
    InvalidVersion(String),

    /// Failed to download update
    #[error("Download failed: {0}")]
    DownloadError(String),

    /// Failed to extract archive
    #[error("Extraction failed: {0}")]
    ExtractionError(String),

    /// Failed to install update (file operations)
    #[error("Installation failed: {0}")]
    InstallError(String),

    /// No suitable release asset found for current platform
    #[error("No compatible release found for platform: {0}")]
    NoCompatibleRelease(String),

    /// Update is already in progress
    #[error("Update already in progress")]
    UpdateInProgress,

    /// Network error
    #[error("Network error: {0}")]
    NetworkError(#[from] reqwest::Error),

    /// IO error
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    /// Checksum verification failed
    #[error("Checksum verification failed: expected {expected}, got {actual}")]
    ChecksumMismatch { expected: String, actual: String },

    /// Checksum file not found in release
    #[error("Checksum file not found in release")]
    ChecksumNotFound,

    /// Invalid binary format (not a valid ELF executable)
    #[error("Invalid binary format: {0}")]
    InvalidBinary(String),
}
