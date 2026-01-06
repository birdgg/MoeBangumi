use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use reqwest::Client;
use sha2::{Digest, Sha256};

use super::error::PosterError;

/// Atomic counter for generating unique temp file suffixes
static TEMP_FILE_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Generate a unique suffix for temporary files.
///
/// Uses atomic counter + timestamp to guarantee uniqueness even under
/// extreme concurrency or clock issues.
fn generate_temp_suffix() -> String {
    let counter = TEMP_FILE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    format!("{}.{}", counter, timestamp)
}

/// Check if a filename matches our temp file pattern.
///
/// Matches files with extension `.tmp.{counter}.{timestamp}` where both
/// counter and timestamp are numeric.
fn is_temp_file(name: &str) -> bool {
    if let Some(pos) = name.rfind(".tmp.") {
        let after_tmp = &name[pos + 5..];
        // Must have content after .tmp. and all chars must be digits or single dot
        if after_tmp.is_empty() {
            return false;
        }
        // Pattern: {counter}.{timestamp} - digits with exactly one dot separator
        let parts: Vec<&str> = after_tmp.split('.').collect();
        return parts.len() == 2
            && !parts[0].is_empty()
            && !parts[1].is_empty()
            && parts[0].chars().all(|c| c.is_ascii_digit())
            && parts[1].chars().all(|c| c.is_ascii_digit());
    }
    false
}

/// A function that asynchronously provides an HTTP client.
/// Used for dynamic proxy configuration support.
pub type ClientProvider = Arc<
    dyn Fn() -> Pin<
            Box<
                dyn Future<Output = Result<Client, Box<dyn std::error::Error + Send + Sync>>>
                    + Send,
            >,
        > + Send
        + Sync,
>;

/// Default URL prefix for local poster paths
const DEFAULT_URL_PREFIX: &str = "/posters/";

/// Service for downloading and managing poster images from BGM.tv.
///
/// Handles poster downloads with path validation, atomic file writes,
/// and automatic deduplication via local file caching.
pub struct PosterService {
    client_provider: Option<ClientProvider>,
    static_client: Option<Client>,
    posters_dir: PathBuf,
    /// URL prefix for local poster paths (e.g., "/posters/" or "/api/posters/")
    url_prefix: String,
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
            url_prefix: DEFAULT_URL_PREFIX.to_string(),
        }
    }

    /// Clean up stale temporary files from previous runs.
    ///
    /// This should be called at startup to remove any `.tmp.*` files that
    /// may have been left behind if the process was terminated during download.
    ///
    /// # Errors
    /// Returns an error if the posters directory cannot be read.
    /// Individual file deletion errors are logged but not propagated.
    pub async fn cleanup_temp_files(&self) -> Result<usize, PosterError> {
        let dir = match tokio::fs::read_dir(&self.posters_dir).await {
            Ok(dir) => dir,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                // Directory doesn't exist yet, nothing to clean up
                return Ok(0);
            }
            Err(e) if e.kind() == std::io::ErrorKind::PermissionDenied => {
                tracing::warn!(
                    "No permission to read posters directory, skipping cleanup: {}",
                    self.posters_dir.display()
                );
                return Ok(0);
            }
            Err(e) => {
                return Err(PosterError::Io {
                    operation: "Failed to read posters directory",
                    path: self.posters_dir.display().to_string(),
                    source: e,
                });
            }
        };

        let mut cleaned = 0;
        let mut entries = dir;

        while let Ok(Some(entry)) = entries.next_entry().await {
            let path = entry.path();
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                // Match files with .tmp.{counter}.{timestamp} extension pattern
                if is_temp_file(name) {
                    match tokio::fs::remove_file(&path).await {
                        Ok(_) => {
                            tracing::debug!("Cleaned up temp file: {}", path.display());
                            cleaned += 1;
                        }
                        Err(e) => {
                            tracing::warn!("Failed to remove temp file {}: {}", path.display(), e);
                        }
                    }
                }
            }
        }

        if cleaned > 0 {
            tracing::info!("Cleaned up {} stale temporary poster files", cleaned);
        }

        Ok(cleaned)
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
            url_prefix: DEFAULT_URL_PREFIX.to_string(),
        }
    }

    /// Create a new PosterService with a custom URL prefix.
    ///
    /// # Arguments
    /// * `provider` - A function that provides an HTTP client asynchronously
    /// * `posters_dir` - Directory to store downloaded posters
    /// * `url_prefix` - Custom URL prefix for local paths (e.g., "/api/posters/")
    pub fn with_url_prefix(
        provider: ClientProvider,
        posters_dir: PathBuf,
        url_prefix: impl Into<String>,
    ) -> Self {
        Self {
            client_provider: Some(provider),
            static_client: None,
            posters_dir,
            url_prefix: url_prefix.into(),
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
    fn local_path_string(&self, filename: &str) -> String {
        format!("{}{}", self.url_prefix, filename)
    }

    /// Check if a URL is already a local path.
    fn is_local_path(&self, url: &str) -> bool {
        url.starts_with(&self.url_prefix)
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

    /// Save bytes to a file atomically.
    ///
    /// Uses a unique temporary file and atomic rename to prevent partial writes
    /// and race conditions. Creates parent directories if needed.
    async fn save_to_file(&self, path: &PathBuf, bytes: &[u8]) -> Result<(), PosterError> {
        // Ensure directory exists
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent)
                .await
                .map_err(|e| PosterError::Io {
                    operation: "Failed to create posters directory",
                    path: parent.display().to_string(),
                    source: e,
                })?;
        }

        // Atomic write: use unique temp file to avoid race conditions
        let temp_suffix = generate_temp_suffix();
        let tmp_path = path.with_extension(format!("tmp.{}", temp_suffix));

        if let Err(e) = tokio::fs::write(&tmp_path, bytes).await {
            // Clean up temp file on failure
            let _ = tokio::fs::remove_file(&tmp_path).await;
            return Err(PosterError::Io {
                operation: "Failed to write poster temp file",
                path: tmp_path.display().to_string(),
                source: e,
            });
        }

        // Rename to final path (atomic on most filesystems)
        if let Err(e) = tokio::fs::rename(&tmp_path, path).await {
            // Clean up temp file on failure
            let _ = tokio::fs::remove_file(&tmp_path).await;
            return Err(PosterError::Io {
                operation: "Failed to rename poster file",
                path: path.display().to_string(),
                source: e,
            });
        }

        Ok(())
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

        // Check if file already exists (optimization to avoid repeat download)
        // Note: TOCTOU race is acceptable here since posters dir is managed by this service
        // and save_to_file uses atomic rename for concurrent write safety
        if tokio::fs::try_exists(&local_path).await.unwrap_or(false) {
            tracing::debug!("Poster already exists: {}", local_path.display());
            return Ok(self.local_path_string(filename));
        }

        tracing::info!("Downloading poster: {}", url);

        let client = self.client().await?;
        let response = client.get(url).send().await?;

        if !response.status().is_success() {
            return Err(PosterError::HttpStatus(response.status().as_u16()));
        }

        let bytes = response.bytes().await?;

        self.save_to_file(&local_path, &bytes).await?;

        Ok(self.local_path_string(filename))
    }

    /// Try to download poster from a URL.
    ///
    /// Wrapper around `download_from_url` that returns `Option` instead of `Result`.
    ///
    /// # Returns
    /// * `Some(local_path)` - Successfully downloaded or already local
    /// * `None` - Download failed
    pub async fn try_download(&self, url: &str) -> Option<String> {
        match self.download_from_url(url).await {
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
    /// Returns (hash, extension_from_url) where extension may be None if not found in URL.
    fn hash_url_to_filename(url: &str) -> (String, Option<String>) {
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
            .map(|ext| ext.to_lowercase());

        (short_hash.to_string(), extension)
    }

    /// Get file extension from Content-Type header.
    fn extension_from_content_type(content_type: &str) -> Option<&'static str> {
        // Parse mime type, ignoring parameters like charset
        let mime = content_type
            .split(';')
            .next()
            .unwrap_or(content_type)
            .trim();
        match mime {
            "image/jpeg" => Some("jpg"),
            "image/png" => Some("png"),
            "image/gif" => Some("gif"),
            "image/webp" => Some("webp"),
            _ => None,
        }
    }

    /// Supported image extensions for poster files.
    const IMAGE_EXTENSIONS: &'static [&'static str] = &["jpg", "jpeg", "png", "gif", "webp"];

    /// Find existing poster file by hash prefix.
    ///
    /// Checks for files matching `{hash}.{ext}` where ext is any supported image extension.
    /// Returns the local path if found, None otherwise.
    async fn find_existing_by_hash(&self, hash: &str) -> Option<String> {
        for ext in Self::IMAGE_EXTENSIONS {
            let filename = format!("{}.{}", hash, ext);
            let path = self.posters_dir.join(&filename);
            if tokio::fs::try_exists(&path).await.unwrap_or(false) {
                tracing::debug!("Found existing poster by hash: {}", path.display());
                return Some(self.local_path_string(&filename));
            }
        }
        None
    }

    /// Download poster from any URL.
    ///
    /// Uses URL hash as filename for deduplication and uniqueness.
    /// For BGM.tv URLs, normalizes first to ensure same image at different sizes
    /// produces the same hash.
    ///
    /// # Returns
    /// * `Ok(local_path)` - Local path like "/posters/abc123def456.jpg"
    /// * `Err` - Download failed
    pub async fn download_from_url(&self, url: &str) -> Result<String, PosterError> {
        // Already a local path
        if self.is_local_path(url) {
            return Ok(url.to_string());
        }

        // Normalize BGM.tv URLs to remove thumbnail size prefix before hashing
        // This ensures same image at different sizes produces the same hash
        let normalized_url = if url.contains("bgm.tv") {
            Self::normalize_bgm_url(url)
        } else {
            url.to_string()
        };

        // Use hash-based filename for all URLs
        let (hash, url_extension) = Self::hash_url_to_filename(&normalized_url);

        // Check if file already exists with any extension (prevents duplicates)
        if let Some(existing_path) = self.find_existing_by_hash(&hash).await {
            return Ok(existing_path);
        }

        // If URL has a valid extension, use it directly
        if let Some(ext) = url_extension {
            let filename = format!("{}.{}", hash, ext);
            return self.download_poster(&normalized_url, &filename).await;
        }

        // No extension in URL, need to detect from Content-Type
        self.download_with_content_type_detection(&normalized_url, &hash)
            .await
    }

    /// Download poster and detect extension from Content-Type header.
    async fn download_with_content_type_detection(
        &self,
        url: &str,
        hash: &str,
    ) -> Result<String, PosterError> {
        tracing::info!("Downloading poster (detecting Content-Type): {}", url);

        let client = self.client().await?;
        let response = client.get(url).send().await?;

        if !response.status().is_success() {
            return Err(PosterError::HttpStatus(response.status().as_u16()));
        }

        // Detect extension from Content-Type header
        let extension = response
            .headers()
            .get(reqwest::header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .and_then(Self::extension_from_content_type)
            .unwrap_or("jpg"); // Fall back to jpg only if Content-Type is missing/unknown

        let filename = format!("{}.{}", hash, extension);
        let local_path = self.posters_dir.join(&filename);

        // Check if file already exists (optimization to avoid reading response body)
        // Note: TOCTOU race is acceptable here since posters dir is managed by this service
        // and save_to_file uses atomic rename for concurrent write safety
        if tokio::fs::try_exists(&local_path).await.unwrap_or(false) {
            tracing::debug!("Poster already exists: {}", local_path.display());
            return Ok(self.local_path_string(&filename));
        }

        let bytes = response.bytes().await?;

        self.save_to_file(&local_path, &bytes).await?;

        tracing::info!(
            "Saved poster (detected as {}): {}",
            extension,
            local_path.display()
        );
        Ok(self.local_path_string(&filename))
    }
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
        let (hash, ext) = PosterService::hash_url_to_filename(url);
        assert_eq!(ext, Some("jpg".to_string()));
        assert_eq!(hash.len(), 16); // 16 hash chars
    }

    #[test]
    fn test_hash_url_to_filename_with_png() {
        let url = "https://example.com/poster.png";
        let (_, ext) = PosterService::hash_url_to_filename(url);
        assert_eq!(ext, Some("png".to_string()));
    }

    #[test]
    fn test_hash_url_to_filename_no_extension() {
        let url = "https://example.com/poster";
        let (hash, ext) = PosterService::hash_url_to_filename(url);
        assert!(ext.is_none()); // No extension found, will use Content-Type detection
        assert_eq!(hash.len(), 16);
    }

    #[test]
    fn test_hash_url_to_filename_invalid_extension() {
        let url = "https://example.com/poster.exe";
        let (hash, ext) = PosterService::hash_url_to_filename(url);
        assert!(ext.is_none()); // Invalid extension, will use Content-Type detection
        assert_eq!(hash.len(), 16);
    }

    #[test]
    fn test_hash_url_to_filename_deterministic() {
        let url = "https://example.com/test.jpg";
        let (hash1, ext1) = PosterService::hash_url_to_filename(url);
        let (hash2, ext2) = PosterService::hash_url_to_filename(url);
        assert_eq!(hash1, hash2); // Same URL produces same hash
        assert_eq!(ext1, ext2);
    }

    #[test]
    fn test_hash_url_to_filename_with_query_params() {
        let url = "https://example.com/poster.jpg?v=123&size=large";
        let (_, ext) = PosterService::hash_url_to_filename(url);
        assert_eq!(ext, Some("jpg".to_string()));
    }

    #[test]
    fn test_hash_url_to_filename_with_fragment() {
        let url = "https://example.com/poster.png#section";
        let (_, ext) = PosterService::hash_url_to_filename(url);
        assert_eq!(ext, Some("png".to_string()));
    }

    #[test]
    fn test_extension_from_content_type() {
        assert_eq!(
            PosterService::extension_from_content_type("image/jpeg"),
            Some("jpg")
        );
        assert_eq!(
            PosterService::extension_from_content_type("image/png"),
            Some("png")
        );
        assert_eq!(
            PosterService::extension_from_content_type("image/gif"),
            Some("gif")
        );
        assert_eq!(
            PosterService::extension_from_content_type("image/webp"),
            Some("webp")
        );
        // With charset parameter
        assert_eq!(
            PosterService::extension_from_content_type("image/png; charset=utf-8"),
            Some("png")
        );
        // Unknown type
        assert_eq!(
            PosterService::extension_from_content_type("text/html"),
            None
        );
    }

    #[test]
    fn test_is_temp_file() {
        // Valid temp file patterns
        assert!(is_temp_file("poster.jpg.tmp.0.1234567890"));
        assert!(is_temp_file("abc123.png.tmp.42.9876543210"));
        assert!(is_temp_file("file.tmp.1.2"));

        // Invalid patterns - should not match
        assert!(!is_temp_file("poster.jpg")); // Normal file
        assert!(!is_temp_file("poster.tmp.jpg")); // .tmp in middle but not our pattern
        assert!(!is_temp_file("file.tmp.")); // Empty after .tmp.
        assert!(!is_temp_file("file.tmp.abc")); // Non-numeric
        assert!(!is_temp_file("file.tmp.123")); // Missing second number part
        assert!(!is_temp_file("file.tmp.123.abc")); // Second part non-numeric
        assert!(!is_temp_file("poster.tmp.whatever.jpg")); // Random .tmp. in name
    }

    #[test]
    fn test_generate_temp_suffix_unique() {
        let suffix1 = generate_temp_suffix();
        let suffix2 = generate_temp_suffix();
        assert_ne!(suffix1, suffix2); // Should be unique due to atomic counter
    }
}
