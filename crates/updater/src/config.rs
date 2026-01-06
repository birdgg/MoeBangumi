//! Configuration for the update service

use std::path::PathBuf;
use std::time::Duration;

/// Configuration for the update service
#[derive(Debug, Clone)]
pub struct UpdateConfig {
    /// GitHub repository owner
    pub repo_owner: String,

    /// GitHub repository name
    pub repo_name: String,

    /// Current version of the application
    pub current_version: String,

    /// Binary name to look for in releases (e.g., "moe")
    pub bin_name: String,

    /// Target directory for downloaded updates (e.g., "/data/bin")
    /// If None, updates will replace the current executable
    pub target_dir: Option<PathBuf>,

    /// How often to check for updates (default: 24 hours)
    pub check_interval: Duration,

    /// Whether to automatically check for updates
    pub auto_check: bool,

    /// Whether to include prereleases
    pub include_prereleases: bool,
}

impl UpdateConfig {
    /// Create a new UpdateConfig with required fields
    pub fn new(
        repo_owner: impl Into<String>,
        repo_name: impl Into<String>,
        current_version: impl Into<String>,
    ) -> Self {
        Self {
            repo_owner: repo_owner.into(),
            repo_name: repo_name.into(),
            current_version: current_version.into(),
            bin_name: "moe".to_string(),
            target_dir: None,
            check_interval: Duration::from_secs(86400), // 24 hours
            auto_check: true,
            include_prereleases: false,
        }
    }

    /// Set the binary name
    pub fn bin_name(mut self, name: impl Into<String>) -> Self {
        self.bin_name = name.into();
        self
    }

    /// Set the target directory for updates
    pub fn target_dir(mut self, path: impl Into<PathBuf>) -> Self {
        self.target_dir = Some(path.into());
        self
    }

    /// Set the check interval
    pub fn check_interval(mut self, interval: Duration) -> Self {
        self.check_interval = interval;
        self
    }

    /// Enable or disable auto-check
    pub fn auto_check(mut self, enabled: bool) -> Self {
        self.auto_check = enabled;
        self
    }

    /// Include or exclude prereleases
    pub fn include_prereleases(mut self, include: bool) -> Self {
        self.include_prereleases = include;
        self
    }

    /// Get the GitHub API URL for releases
    pub fn releases_url(&self) -> String {
        format!(
            "https://api.github.com/repos/{}/{}/releases",
            self.repo_owner, self.repo_name
        )
    }

    /// Get the asset name pattern for current platform
    pub fn asset_pattern(&self) -> String {
        let arch = if cfg!(target_arch = "x86_64") {
            "amd64"
        } else if cfg!(target_arch = "aarch64") {
            "arm64"
        } else {
            "unknown"
        };

        let os = if cfg!(target_os = "linux") {
            "linux"
        } else if cfg!(target_os = "macos") {
            "darwin"
        } else if cfg!(target_os = "windows") {
            "windows"
        } else {
            "unknown"
        };

        format!("{}-{}-{}", self.bin_name, os, arch)
    }
}

impl Default for UpdateConfig {
    fn default() -> Self {
        Self::new("birdgg", "moe-bangumi", "0.0.0")
    }
}
