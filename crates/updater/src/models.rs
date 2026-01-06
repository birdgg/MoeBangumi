//! Data models for update functionality

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Version information including current and latest versions
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(utoipa::ToSchema))]
pub struct VersionInfo {
    /// Current running version
    pub current: String,

    /// Latest available version (if checked)
    pub latest: Option<String>,

    /// Whether an update is available
    pub update_available: bool,

    /// URL to the release page
    pub release_url: Option<String>,

    /// Release changelog/body
    pub changelog: Option<String>,

    /// When the latest version was published
    pub published_at: Option<DateTime<Utc>>,

    /// When we last checked for updates
    pub last_checked: Option<DateTime<Utc>>,

    /// Current update status
    pub status: UpdateStatus,
}

impl VersionInfo {
    /// Create a new VersionInfo with the current version
    pub fn new(current: impl Into<String>) -> Self {
        Self {
            current: current.into(),
            latest: None,
            update_available: false,
            release_url: None,
            changelog: None,
            published_at: None,
            last_checked: None,
            status: UpdateStatus::Idle,
        }
    }
}

/// Current status of the update process
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
#[cfg_attr(feature = "openapi", derive(utoipa::ToSchema))]
pub enum UpdateStatus {
    /// No update in progress
    #[default]
    Idle,

    /// Checking for updates
    Checking,

    /// Update available, not yet downloading
    Available,

    /// Downloading update
    Downloading,

    /// Update downloaded, restart required
    RestartRequired,

    /// Update failed
    Failed,
}

/// Information about a GitHub release
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReleaseInfo {
    /// Version tag (e.g., "v0.2.5")
    pub tag_name: String,

    /// Release name/title
    pub name: String,

    /// Release body/changelog
    pub body: Option<String>,

    /// URL to the release page
    pub html_url: String,

    /// When the release was published
    pub published_at: DateTime<Utc>,

    /// Whether this is a prerelease
    pub prerelease: bool,

    /// Whether this is a draft
    pub draft: bool,

    /// Release assets (downloadable files)
    pub assets: Vec<ReleaseAsset>,
}

/// A downloadable asset from a GitHub release
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReleaseAsset {
    /// Asset name (e.g., "moe-linux-amd64.tar.gz")
    pub name: String,

    /// Download URL
    pub browser_download_url: String,

    /// File size in bytes
    pub size: u64,

    /// Content type
    pub content_type: String,
}

impl ReleaseInfo {
    /// Get the version without the "v" prefix
    pub fn version(&self) -> &str {
        self.tag_name.strip_prefix('v').unwrap_or(&self.tag_name)
    }

    /// Find an asset matching the given pattern
    pub fn find_asset(&self, pattern: &str) -> Option<&ReleaseAsset> {
        self.assets.iter().find(|a| a.name.contains(pattern))
    }
}
