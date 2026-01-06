//! Update service implementation

use std::path::PathBuf;
use std::sync::Arc;

use chrono::Utc;
use parking_lot::RwLock;
use semver::Version;
use tokio::sync::mpsc;

use crate::config::UpdateConfig;
use crate::error::UpdateError;
use crate::models::{ReleaseAsset, ReleaseInfo, UpdateStatus, VersionInfo};

/// Messages for the update service actor
#[derive(Debug)]
pub enum UpdateMessage {
    /// Check for updates (and auto-update if available)
    CheckForUpdates,
    /// Shutdown the service
    Shutdown,
}

/// Handle to interact with the update service
#[derive(Clone)]
pub struct UpdateServiceHandle {
    sender: mpsc::Sender<UpdateMessage>,
    state: Arc<RwLock<VersionInfo>>,
}

impl UpdateServiceHandle {
    /// Get current version info
    pub fn get_version_info(&self) -> VersionInfo {
        self.state.read().clone()
    }

    /// Trigger a manual check for updates
    pub async fn check_for_updates(&self) -> Result<(), UpdateError> {
        self.sender
            .send(UpdateMessage::CheckForUpdates)
            .await
            .map_err(|_| UpdateError::FetchError("Service not running".to_string()))
    }

    /// Shutdown the service
    pub async fn shutdown(&self) {
        let _ = self.sender.send(UpdateMessage::Shutdown).await;
    }
}

/// Update service that handles version checking and updates
pub struct UpdateService {
    config: UpdateConfig,
    http_client: reqwest::Client,
    state: Arc<RwLock<VersionInfo>>,
    receiver: mpsc::Receiver<UpdateMessage>,
}

impl UpdateService {
    /// Create a new update service with a shared HTTP client
    ///
    /// The HTTP client should be passed from the application's HttpClientService
    /// to ensure proxy settings are respected.
    pub fn new(config: UpdateConfig, http_client: reqwest::Client) -> (UpdateServiceHandle, Self) {
        let (sender, receiver) = mpsc::channel(16);

        let state = Arc::new(RwLock::new(VersionInfo::new(&config.current_version)));

        let handle = UpdateServiceHandle {
            sender,
            state: Arc::clone(&state),
        };

        let service = Self {
            config,
            http_client,
            state,
            receiver,
        };

        (handle, service)
    }

    /// Run the update service (call this in a tokio task)
    pub async fn run(mut self) {
        let mut check_interval = tokio::time::interval(self.config.check_interval);
        check_interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

        // Initial check on startup
        if self.config.auto_check {
            self.check_for_updates().await;
        }

        loop {
            tokio::select! {
                _ = check_interval.tick(), if self.config.auto_check => {
                    self.check_and_auto_update().await;
                }
                msg = self.receiver.recv() => {
                    match msg {
                        Some(UpdateMessage::CheckForUpdates) => {
                            self.check_and_auto_update().await;
                        }
                        Some(UpdateMessage::Shutdown) | None => {
                            tracing::info!("Update service shutting down");
                            break;
                        }
                    }
                }
            }
        }
    }

    /// Check for available updates and auto-update if found
    async fn check_and_auto_update(&self) {
        if self.check_for_updates().await {
            // Update found, automatically download and install
            self.perform_update().await;
        }
    }

    /// Check for available updates, returns true if update is available
    async fn check_for_updates(&self) -> bool {
        tracing::debug!("Checking for updates...");

        // Update status to checking
        {
            let mut state = self.state.write();
            state.status = UpdateStatus::Checking;
        }

        match self.fetch_latest_release().await {
            Ok(Some(release)) => {
                let update_available = self.process_release(release);
                update_available
            }
            Ok(None) => {
                tracing::debug!("No releases found");
                let mut state = self.state.write();
                state.last_checked = Some(Utc::now());
                state.status = UpdateStatus::Idle;
                false
            }
            Err(e) => {
                tracing::warn!("Failed to check for updates: {}", e);
                let mut state = self.state.write();
                state.status = UpdateStatus::Failed;
                false
            }
        }
    }

    /// Fetch the latest release from GitHub
    async fn fetch_latest_release(&self) -> Result<Option<ReleaseInfo>, UpdateError> {
        let url = format!("{}/latest", self.config.releases_url());

        let response = self
            .http_client
            .get(&url)
            .header("Accept", "application/vnd.github.v3+json")
            .header(
                "User-Agent",
                format!("{}/{}", self.config.bin_name, self.config.current_version),
            )
            .send()
            .await?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }

        if !response.status().is_success() {
            return Err(UpdateError::FetchError(format!(
                "GitHub API returned {}",
                response.status()
            )));
        }

        let release: ReleaseInfo = response
            .json()
            .await
            .map_err(|e| UpdateError::FetchError(e.to_string()))?;

        // Skip drafts and prereleases (unless configured otherwise)
        if release.draft || (release.prerelease && !self.config.include_prereleases) {
            return Ok(None);
        }

        Ok(Some(release))
    }

    /// Process a release and update state, returns true if update is available
    fn process_release(&self, release: ReleaseInfo) -> bool {
        let latest_version = release.version();

        let current = Version::parse(&self.config.current_version).unwrap_or(Version::new(0, 0, 0));

        let latest = match Version::parse(latest_version) {
            Ok(v) => v,
            Err(_) => {
                tracing::warn!("Invalid version format: {}", latest_version);
                return false;
            }
        };

        let update_available = latest > current;

        let mut state = self.state.write();
        state.latest = Some(latest.to_string());
        state.update_available = update_available;
        state.release_url = Some(release.html_url);
        state.changelog = release.body;
        state.published_at = Some(release.published_at);
        state.last_checked = Some(Utc::now());
        state.status = if update_available {
            UpdateStatus::Available
        } else {
            UpdateStatus::Idle
        };

        if update_available {
            tracing::info!(
                "New version available: {} -> {}",
                self.config.current_version,
                latest
            );
        } else {
            tracing::debug!("Already on latest version: {}", current);
        }

        update_available
    }

    /// Perform the update and restart
    async fn perform_update(&self) {
        tracing::info!("Starting auto-update...");

        // Check if update is available
        {
            let state = self.state.read();
            if !state.update_available {
                tracing::info!("No update available");
                return;
            }
            if state.status == UpdateStatus::Downloading {
                tracing::warn!("Update already in progress");
                return;
            }
        }

        // Set status to downloading
        {
            let mut state = self.state.write();
            state.status = UpdateStatus::Downloading;
        }

        // Fetch release info again to get asset URLs
        let release = match self.fetch_latest_release().await {
            Ok(Some(r)) => r,
            Ok(None) => {
                self.set_failed_status();
                return;
            }
            Err(e) => {
                tracing::error!("Failed to fetch release: {}", e);
                self.set_failed_status();
                return;
            }
        };

        // Find the right asset for this platform
        let asset_pattern = self.config.asset_pattern();
        let asset = match release.find_asset(&asset_pattern) {
            Some(a) => a,
            None => {
                tracing::error!("No asset found matching pattern: {}", asset_pattern);
                self.set_failed_status();
                return;
            }
        };

        // Download and install with checksum verification
        if let Err(e) = self.download_and_install(asset, &release).await {
            tracing::error!("Update failed: {}", e);
            self.set_failed_status();
            return;
        }

        // Success - restart
        tracing::info!("Update downloaded successfully. Restarting...");
        // Give a moment for any pending operations
        tokio::time::sleep(std::time::Duration::from_secs(1)).await;
        // Exit with code 0 - Docker will restart the container
        std::process::exit(0);
    }

    /// Download and install the update with checksum verification
    async fn download_and_install(
        &self,
        asset: &ReleaseAsset,
        release: &ReleaseInfo,
    ) -> Result<(), UpdateError> {
        tracing::info!("Downloading {} ({} bytes)...", asset.name, asset.size);

        // Determine target path
        let target_dir = self
            .config
            .target_dir
            .clone()
            .unwrap_or_else(|| PathBuf::from("/data/bin"));

        // Ensure target directory exists
        tokio::fs::create_dir_all(&target_dir).await?;

        let temp_file = target_dir.join(format!("{}.tmp", self.config.bin_name));
        let final_file = target_dir.join(&self.config.bin_name);

        // Fetch expected checksum first
        let expected_hash = self.fetch_checksum(release, &asset.name).await?;
        tracing::debug!("Expected checksum for {}: {}", asset.name, expected_hash);

        // Download the asset
        let response = self
            .http_client
            .get(&asset.browser_download_url)
            .header(
                "User-Agent",
                format!("{}/{}", self.config.bin_name, self.config.current_version),
            )
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(UpdateError::DownloadError(format!(
                "Download failed with status {}",
                response.status()
            )));
        }

        let bytes = response.bytes().await?;

        // Verify checksum before writing to disk
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(&bytes);
        let actual_hash = hex::encode(hasher.finalize());

        if actual_hash.to_lowercase() != expected_hash.to_lowercase() {
            return Err(UpdateError::ChecksumMismatch {
                expected: expected_hash,
                actual: actual_hash,
            });
        }
        tracing::info!("Checksum verified successfully");

        // Save to temp file first
        tokio::fs::write(&temp_file, &bytes).await?;

        // Extract if it's an archive
        if asset.name.ends_with(".tar.gz") || asset.name.ends_with(".tgz") {
            self.extract_tar_gz(&temp_file, &target_dir).await?;
            // Remove the temp archive
            let _ = tokio::fs::remove_file(&temp_file).await;
        } else if asset.name.ends_with(".zip") {
            // For zip files, we'd need to handle differently
            return Err(UpdateError::ExtractionError(
                "ZIP extraction not implemented".to_string(),
            ));
        } else {
            // Assume it's a raw binary
            tokio::fs::rename(&temp_file, &final_file).await?;
        }

        // Validate ELF binary format and set executable permissions (Unix only)
        #[cfg(unix)]
        {
            self.validate_and_set_permissions(&final_file).await?;
        }

        tracing::info!("Update installed to {:?}", final_file);
        Ok(())
    }

    /// Extract a tar.gz archive
    async fn extract_tar_gz(
        &self,
        archive_path: &PathBuf,
        target_dir: &PathBuf,
    ) -> Result<(), UpdateError> {
        use std::io::Read;

        let archive_path = archive_path.clone();
        let target_dir = target_dir.clone();
        let bin_name = self.config.bin_name.clone();

        // Run extraction in blocking task
        tokio::task::spawn_blocking(move || {
            let file =
                std::fs::File::open(&archive_path).map_err(|e| UpdateError::IoError(e))?;

            let decoder = flate2::read::GzDecoder::new(file);
            let mut archive = tar::Archive::new(decoder);

            for entry in archive
                .entries()
                .map_err(|e| UpdateError::ExtractionError(e.to_string()))?
            {
                let mut entry =
                    entry.map_err(|e| UpdateError::ExtractionError(e.to_string()))?;

                let path = entry
                    .path()
                    .map_err(|e| UpdateError::ExtractionError(e.to_string()))?;

                // Look for the binary file
                if let Some(name) = path.file_name() {
                    if name.to_string_lossy() == bin_name {
                        let target_path = target_dir.join(&bin_name);
                        let mut content = Vec::new();
                        entry
                            .read_to_end(&mut content)
                            .map_err(|e| UpdateError::ExtractionError(e.to_string()))?;

                        // Validate ELF magic before writing
                        const ELF_MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];
                        if content.len() < 4 || content[..4] != ELF_MAGIC {
                            return Err(UpdateError::InvalidBinary(format!(
                                "Not a valid ELF executable in archive"
                            )));
                        }

                        std::fs::write(&target_path, content)?;

                        // Set restrictive permissions
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            let mut perms = std::fs::metadata(&target_path)?.permissions();
                            perms.set_mode(0o750);
                            std::fs::set_permissions(&target_path, perms)?;
                        }

                        tracing::debug!("Extracted and validated {} to {:?}", bin_name, target_path);
                        return Ok(());
                    }
                }
            }

            Err(UpdateError::ExtractionError(format!(
                "Binary '{}' not found in archive",
                bin_name
            )))
        })
        .await
        .map_err(|e| UpdateError::ExtractionError(e.to_string()))?
    }

    /// Fetch checksum for an asset from checksums.txt
    async fn fetch_checksum(
        &self,
        release: &ReleaseInfo,
        asset_name: &str,
    ) -> Result<String, UpdateError> {
        // Find checksums.txt asset
        let checksum_asset = release
            .find_asset("checksums.txt")
            .ok_or(UpdateError::ChecksumNotFound)?;

        tracing::debug!("Downloading checksums.txt from {}", checksum_asset.browser_download_url);

        // Download checksums.txt
        let response = self
            .http_client
            .get(&checksum_asset.browser_download_url)
            .header(
                "User-Agent",
                format!("{}/{}", self.config.bin_name, self.config.current_version),
            )
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(UpdateError::ChecksumNotFound);
        }

        let content = response.text().await?;

        // Parse format: "sha256hash  filename" (two spaces between hash and filename)
        for line in content.lines() {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 && parts[1] == asset_name {
                return Ok(parts[0].to_lowercase());
            }
        }

        tracing::warn!(
            "Checksum for {} not found in checksums.txt. Contents:\n{}",
            asset_name,
            content
        );
        Err(UpdateError::ChecksumNotFound)
    }

    /// Set status to failed
    fn set_failed_status(&self) {
        let mut state = self.state.write();
        state.status = UpdateStatus::Failed;
    }

    /// Validate ELF binary format and set executable permissions
    #[cfg(unix)]
    async fn validate_and_set_permissions(&self, path: &PathBuf) -> Result<(), UpdateError> {
        use std::os::unix::fs::PermissionsExt;

        // Read the first 4 bytes to check ELF magic number
        let mut file = tokio::fs::File::open(path).await?;
        let mut magic = [0u8; 4];
        use tokio::io::AsyncReadExt;
        file.read_exact(&mut magic).await?;

        // ELF magic number: 0x7F 'E' 'L' 'F'
        const ELF_MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];
        if magic != ELF_MAGIC {
            return Err(UpdateError::InvalidBinary(format!(
                "Not a valid ELF executable (magic: {:02x?})",
                magic
            )));
        }

        tracing::debug!("ELF binary validation passed for {:?}", path);

        // Set more restrictive permissions (owner: rwx, group: rx, others: none)
        let mut perms = tokio::fs::metadata(path).await?.permissions();
        perms.set_mode(0o750);
        tokio::fs::set_permissions(path, perms).await?;

        tracing::debug!("Set permissions to 0750 for {:?}", path);
        Ok(())
    }
}
