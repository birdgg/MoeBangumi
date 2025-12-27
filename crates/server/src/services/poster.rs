use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;

use reqwest::Client;
use thiserror::Error;

/// A function that asynchronously provides an HTTP client.
/// Used for dynamic proxy configuration support.
pub type ClientProvider = Arc<
    dyn Fn() -> Pin<Box<dyn Future<Output = Result<Client, Box<dyn std::error::Error + Send + Sync>>> + Send>>
        + Send
        + Sync,
>;

/// Errors that can occur when downloading or managing posters.
#[derive(Debug, Error)]
pub enum PosterError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Invalid poster path: {0}")]
    InvalidPath(String),

    #[error("HTTP error: {0}")]
    HttpStatus(u16),

    #[error("HTTP client error: {0}")]
    HttpClient(String),
}

/// Service for downloading and managing poster images from BGM.tv.
///
/// Handles poster downloads with path validation, atomic file writes,
/// and automatic deduplication via local file caching.
pub struct PosterService {
    client_provider: Option<ClientProvider>,
    static_client: Option<Client>,
    posters_dir: PathBuf,
}

impl PosterService {
    /// Create a new PosterService with a static HTTP client.
    ///
    /// # Arguments
    /// * `http_client` - Shared reqwest client for HTTP requests
    /// * `posters_dir` - Directory to store downloaded posters
    pub fn new(http_client: Client, posters_dir: PathBuf) -> Self {
        Self {
            client_provider: None,
            static_client: Some(http_client),
            posters_dir,
        }
    }

    /// Create a new PosterService with a dynamic client provider.
    ///
    /// # Arguments
    /// * `provider` - A function that provides an HTTP client asynchronously
    /// * `posters_dir` - Directory to store downloaded posters
    pub fn with_client_provider(provider: ClientProvider, posters_dir: PathBuf) -> Self {
        Self {
            client_provider: Some(provider),
            static_client: None,
            posters_dir,
        }
    }

    /// Get the HTTP client for making requests.
    async fn client(&self) -> Result<Client, PosterError> {
        if let Some(provider) = &self.client_provider {
            provider()
                .await
                .map_err(|e| PosterError::HttpClient(e.to_string()))
        } else if let Some(client) = &self.static_client {
            Ok(client.clone())
        } else {
            Err(PosterError::HttpClient(
                "No HTTP client configured".to_string(),
            ))
        }
    }

    /// Build the local path string for a given filename.
    fn local_path_string(filename: &str) -> String {
        format!("/posters/{}", filename)
    }

    /// Validate filename to prevent path traversal attacks.
    fn validate_filename(filename: &str) -> Result<(), PosterError> {
        if filename.contains("..") || filename.contains('/') {
            return Err(PosterError::InvalidPath(format!(
                "Invalid filename: {}",
                filename
            )));
        }
        Ok(())
    }

    /// Extract filename from a BGM.tv poster URL.
    ///
    /// Handles:
    /// - Full BGM.tv URL: "https://lain.bgm.tv/pic/cover/l/de/4a/329906_hmtVD.jpg" -> "329906_hmtVD.jpg"
    /// - Local path: "/posters/abc.jpg" -> None (already local)
    fn extract_bgm_filename(url: &str) -> Option<&str> {
        if url.starts_with("/posters/") {
            return None;
        }

        if url.contains("bgm.tv") {
            // Extract filename from the last path segment
            url.rsplit_once('/').map(|(_, filename)| filename)
        } else {
            None
        }
    }

    /// Download a poster from a URL and return the local path.
    ///
    /// # Arguments
    /// * `url` - Full URL to download from
    /// * `filename` - Filename to save as
    ///
    /// # Returns
    /// * `Ok(String)` - Local path relative to data dir (e.g., "/posters/329906_hmtVD.jpg")
    /// * `Err` - Download or save failed
    pub async fn download_poster(&self, url: &str, filename: &str) -> Result<String, PosterError> {
        Self::validate_filename(filename)?;
        let local_path = self.posters_dir.join(filename);

        // Check if file already exists (avoid repeat download)
        if local_path.exists() {
            tracing::debug!("Poster already exists: {}", local_path.display());
            return Ok(Self::local_path_string(filename));
        }

        tracing::info!("Downloading poster: {}", url);

        let client = self.client().await?;
        let response = client.get(url).send().await?;

        if !response.status().is_success() {
            return Err(PosterError::HttpStatus(response.status().as_u16()));
        }

        let bytes = response.bytes().await?;

        // Ensure directory exists
        if let Some(parent) = local_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        // Atomic write: use unique temp file to avoid race conditions
        let temp_suffix = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let tmp_path = local_path.with_extension(format!("tmp.{}", temp_suffix));

        tokio::fs::write(&tmp_path, &bytes).await?;

        // Rename to final path (atomic on most filesystems)
        if let Err(e) = tokio::fs::rename(&tmp_path, &local_path).await {
            // Clean up temp file on failure
            let _ = tokio::fs::remove_file(&tmp_path).await;
            return Err(e.into());
        }

        tracing::info!("Saved poster: {}", local_path.display());
        Ok(Self::local_path_string(filename))
    }

    /// Try to download poster from a URL.
    ///
    /// Handles different URL formats:
    /// - BGM.tv URL: "https://lain.bgm.tv/pic/cover/l/de/4a/329906_hmtVD.jpg"
    /// - Local path: "/posters/abc.jpg" (returns as-is)
    ///
    /// # Returns
    /// * `Some(local_path)` - Successfully downloaded or already local
    /// * `None` - Not a BGM.tv URL or download failed
    pub async fn try_download(&self, url: &str) -> Option<String> {
        // Already a local path
        if url.starts_with("/posters/") {
            tracing::debug!("URL is already a local path: {}", url);
            return Some(url.to_string());
        }

        let filename = Self::extract_bgm_filename(url)?;

        match self.download_poster(url, filename).await {
            Ok(local_path) => Some(local_path),
            Err(e) => {
                tracing::warn!("Failed to download poster: {}", e);
                None
            }
        }
    }
}
