//! Torrent creation request.

use thiserror::Error;

use super::SubtitleType;

/// Request to create a new Torrent entity.
#[derive(Debug, Clone)]
pub struct CreateTorrentRequest {
    pub bangumi_id: i64,
    pub rss_id: Option<i64>,
    pub info_hash: String,
    pub torrent_url: String,
    pub episode_number: Option<i32>,
    pub subtitle_group: Option<String>,
    pub subtitle_languages: Vec<SubtitleType>,
    pub resolution: Option<String>,
}

/// Error when creating a Torrent entity.
#[derive(Debug, Error)]
pub enum CreateTorrentError {
    /// Info hash cannot be empty.
    #[error("Info hash cannot be empty")]
    EmptyInfoHash,
    /// Info hash has invalid format (must be hex, 40 or 64 chars).
    #[error("Invalid info hash format: must be 40 or 64 hex characters")]
    InvalidInfoHashFormat,
    /// Torrent URL cannot be empty.
    #[error("Torrent URL cannot be empty")]
    EmptyTorrentUrl,
}

impl CreateTorrentRequest {
    /// Create a new request with validation.
    pub fn new(
        bangumi_id: i64,
        info_hash: &str,
        torrent_url: &str,
    ) -> Result<Self, CreateTorrentError> {
        let info_hash = info_hash.trim().to_lowercase();
        if info_hash.is_empty() {
            return Err(CreateTorrentError::EmptyInfoHash);
        }

        // Validate info hash format (40 chars for v1, 64 for v2)
        if (info_hash.len() != 40 && info_hash.len() != 64)
            || !info_hash.chars().all(|c| c.is_ascii_hexdigit())
        {
            return Err(CreateTorrentError::InvalidInfoHashFormat);
        }

        let torrent_url = torrent_url.trim();
        if torrent_url.is_empty() {
            return Err(CreateTorrentError::EmptyTorrentUrl);
        }

        Ok(Self {
            bangumi_id,
            rss_id: None,
            info_hash,
            torrent_url: torrent_url.to_string(),
            episode_number: None,
            subtitle_group: None,
            subtitle_languages: Vec::new(),
            resolution: None,
        })
    }

    /// Set the RSS ID.
    pub fn with_rss_id(mut self, rss_id: i64) -> Self {
        self.rss_id = Some(rss_id);
        self
    }

    /// Set the episode number.
    pub fn with_episode_number(mut self, episode: i32) -> Self {
        self.episode_number = Some(episode);
        self
    }

    /// Set the subtitle group.
    pub fn with_subtitle_group(mut self, group: String) -> Self {
        self.subtitle_group = Some(group);
        self
    }

    /// Set the subtitle languages.
    pub fn with_subtitle_languages(mut self, languages: Vec<SubtitleType>) -> Self {
        self.subtitle_languages = languages;
        self
    }

    /// Set the resolution.
    pub fn with_resolution(mut self, resolution: String) -> Self {
        self.resolution = Some(resolution);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_torrent_request_valid() {
        let request = CreateTorrentRequest::new(
            1,
            "a".repeat(40).as_str(),
            "magnet:?xt=urn:btih:abc",
        );
        assert!(request.is_ok());
    }

    #[test]
    fn test_create_torrent_request_invalid_hash() {
        let request =
            CreateTorrentRequest::new(1, "invalid", "magnet:?xt=urn:btih:abc");
        assert!(matches!(
            request,
            Err(CreateTorrentError::InvalidInfoHashFormat)
        ));
    }
}
