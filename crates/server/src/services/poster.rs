mod actor;

use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;

use reqwest::Client;
use sha2::{Digest, Sha256};
use sqlx::SqlitePool;
use thiserror::Error;
use tokio::sync::mpsc;

pub use actor::PosterDownloadHandle;
use actor::PosterDownloadActor;

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

    #[error("{operation} '{path}': {source}")]
    Io {
        operation: &'static str,
        path: String,
        source: std::io::Error,
    },

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

    /// Normalize a BGM.tv URL by removing thumbnail size prefix.
    ///
    /// Converts:
    /// - "https://lain.bgm.tv/r/400/pic/cover/l/3c/82/373267_ffBO8.jpg"
    /// - to "https://lain.bgm.tv/pic/cover/l/3c/82/373267_ffBO8.jpg"
    fn normalize_bgm_url(url: &str) -> String {
        // Pattern: /r/{size}/ where size is a number
        if let Some(idx) = url.find("/r/") {
            // Find the end of /r/{size}/ pattern
            let after_r = &url[idx + 3..]; // skip "/r/"
            if let Some(slash_idx) = after_r.find('/') {
                // Check if the part between /r/ and next / is a number
                let size_part = &after_r[..slash_idx];
                if size_part.chars().all(|c| c.is_ascii_digit()) {
                    // Remove /r/{size} part
                    let before = &url[..idx];
                    let after = &after_r[slash_idx..];
                    return format!("{}{}", before, after);
                }
            }
        }
        url.to_string()
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
            tokio::fs::create_dir_all(parent).await.map_err(|e| {
                PosterError::Io {
                    operation: "Failed to create posters directory",
                    path: parent.display().to_string(),
                    source: e,
                }
            })?;
        }

        // Atomic write: use unique temp file to avoid race conditions
        let temp_suffix = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let tmp_path = local_path.with_extension(format!("tmp.{}", temp_suffix));

        if let Err(e) = tokio::fs::write(&tmp_path, &bytes).await {
            // Clean up temp file on failure
            let _ = tokio::fs::remove_file(&tmp_path).await;
            return Err(PosterError::Io {
                operation: "Failed to write poster temp file",
                path: tmp_path.display().to_string(),
                source: e,
            });
        }

        // Rename to final path (atomic on most filesystems)
        if let Err(e) = tokio::fs::rename(&tmp_path, &local_path).await {
            // Clean up temp file on failure
            let _ = tokio::fs::remove_file(&tmp_path).await;
            return Err(PosterError::Io {
                operation: "Failed to rename poster file",
                path: local_path.display().to_string(),
                source: e,
            });
        }

        tracing::info!("Saved poster: {}", local_path.display());
        Ok(Self::local_path_string(filename))
    }

    /// Try to download poster from a URL.
    ///
    /// Handles different URL formats:
    /// - BGM.tv URL: "https://lain.bgm.tv/pic/cover/l/de/4a/329906_hmtVD.jpg"
    /// - BGM.tv thumbnail URL: "https://lain.bgm.tv/r/400/pic/cover/l/3c/82/373267_ffBO8.jpg" (normalized to full size)
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

        // Normalize BGM.tv URL to remove thumbnail size prefix
        let normalized_url = Self::normalize_bgm_url(url);
        let filename = Self::extract_bgm_filename(&normalized_url)?;

        match self.download_poster(&normalized_url, filename).await {
            Ok(local_path) => Some(local_path),
            Err(e) => {
                tracing::warn!("Failed to download poster: {}", e);
                None
            }
        }
    }

    /// Generate a filename from URL using SHA256 hash.
    ///
    /// Extracts the extension from the URL and appends it to the hash.
    /// Falls back to .jpg if no valid extension found.
    fn hash_url_to_filename(url: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(url.as_bytes());
        let hash = hex::encode(hasher.finalize());
        let short_hash = &hash[..16]; // Use first 16 chars for shorter filename

        // Extract extension from URL, handling query params and fragments
        let extension = url
            .rsplit('/')
            .next()
            .and_then(|filename| {
                // Remove query params and fragment before extracting extension
                let clean_filename = filename.split('?').next().unwrap_or(filename);
                let clean_filename = clean_filename.split('#').next().unwrap_or(clean_filename);
                clean_filename.rsplit('.').next()
            })
            .filter(|ext| {
                let ext_lower = ext.to_lowercase();
                matches!(ext_lower.as_str(), "jpg" | "jpeg" | "png" | "gif" | "webp")
            })
            .unwrap_or("jpg");

        format!("{}.{}", short_hash, extension)
    }

    /// Download poster from any URL (not just BGM.tv).
    ///
    /// Uses URL hash as filename for deduplication.
    ///
    /// # Returns
    /// * `Ok(local_path)` - Local path like "/posters/abc123.jpg"
    /// * `Err` - Download failed
    pub async fn download_from_url(&self, url: &str) -> Result<String, PosterError> {
        // Already a local path
        if url.starts_with("/posters/") {
            return Ok(url.to_string());
        }

        // For BGM.tv URLs, normalize first then use original filename
        if url.contains("bgm.tv") {
            let normalized_url = Self::normalize_bgm_url(url);
            if let Some(filename) = Self::extract_bgm_filename(&normalized_url) {
                return self.download_poster(&normalized_url, filename).await;
            }
        }

        // For other URLs, use hash-based filename
        let filename = Self::hash_url_to_filename(url);
        self.download_poster(url, &filename).await
    }
}

/// Create the PosterDownload actor service.
///
/// Returns a `PosterDownloadHandle` that can be used to submit download tasks.
/// The actor runs in the background and processes downloads asynchronously
/// with a fixed concurrency limit of 5.
pub fn create_poster_download_service(
    db: SqlitePool,
    poster: Arc<PosterService>,
) -> PosterDownloadHandle {
    let (sender, receiver) = mpsc::channel(100);

    let actor = PosterDownloadActor::new(db, poster, receiver);
    tokio::spawn(actor.run());

    PosterDownloadHandle::new(sender)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_bgm_url_with_thumbnail() {
        let url = "https://lain.bgm.tv/r/400/pic/cover/l/3c/82/373267_ffBO8.jpg";
        let expected = "https://lain.bgm.tv/pic/cover/l/3c/82/373267_ffBO8.jpg";
        assert_eq!(PosterService::normalize_bgm_url(url), expected);
    }

    #[test]
    fn test_normalize_bgm_url_without_thumbnail() {
        let url = "https://lain.bgm.tv/pic/cover/l/de/4a/329906_hmtVD.jpg";
        assert_eq!(PosterService::normalize_bgm_url(url), url);
    }

    #[test]
    fn test_normalize_bgm_url_different_sizes() {
        // Test with different thumbnail sizes
        let url_200 = "https://lain.bgm.tv/r/200/pic/cover/l/3c/82/373267_ffBO8.jpg";
        let url_800 = "https://lain.bgm.tv/r/800/pic/cover/l/3c/82/373267_ffBO8.jpg";
        let expected = "https://lain.bgm.tv/pic/cover/l/3c/82/373267_ffBO8.jpg";

        assert_eq!(PosterService::normalize_bgm_url(url_200), expected);
        assert_eq!(PosterService::normalize_bgm_url(url_800), expected);
    }

    #[test]
    fn test_normalize_bgm_url_non_bgm() {
        let url = "https://mikanani.me/images/Bangumi/202501/b95edb86.jpg";
        assert_eq!(PosterService::normalize_bgm_url(url), url);
    }

    #[test]
    fn test_hash_url_to_filename_with_jpg() {
        let url = "https://mikanani.me/images/Bangumi/202501/b95edb86.jpg";
        let filename = PosterService::hash_url_to_filename(url);
        assert!(filename.ends_with(".jpg"));
        assert_eq!(filename.len(), 20); // 16 hash chars + 4 for ".jpg"
    }

    #[test]
    fn test_hash_url_to_filename_with_png() {
        let url = "https://example.com/poster.png";
        let filename = PosterService::hash_url_to_filename(url);
        assert!(filename.ends_with(".png"));
    }

    #[test]
    fn test_hash_url_to_filename_no_extension() {
        let url = "https://example.com/poster";
        let filename = PosterService::hash_url_to_filename(url);
        assert!(filename.ends_with(".jpg")); // Falls back to jpg
    }

    #[test]
    fn test_hash_url_to_filename_invalid_extension() {
        let url = "https://example.com/poster.exe";
        let filename = PosterService::hash_url_to_filename(url);
        assert!(filename.ends_with(".jpg")); // Falls back to jpg
    }

    #[test]
    fn test_hash_url_to_filename_deterministic() {
        let url = "https://example.com/test.jpg";
        let filename1 = PosterService::hash_url_to_filename(url);
        let filename2 = PosterService::hash_url_to_filename(url);
        assert_eq!(filename1, filename2); // Same URL produces same filename
    }

    #[test]
    fn test_hash_url_to_filename_with_query_params() {
        let url = "https://example.com/poster.jpg?v=123&size=large";
        let filename = PosterService::hash_url_to_filename(url);
        assert!(filename.ends_with(".jpg"));
    }

    #[test]
    fn test_hash_url_to_filename_with_fragment() {
        let url = "https://example.com/poster.png#section";
        let filename = PosterService::hash_url_to_filename(url);
        assert!(filename.ends_with(".png"));
    }
}
