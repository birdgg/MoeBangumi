//! Metadata creation request.

use thiserror::Error;

use super::{EpisodeOffset, Platform, Season, SeasonError};

/// Request to create a new Metadata entity.
#[derive(Debug, Clone)]
pub struct CreateMetadataRequest {
    pub title_chinese: String,
    pub title_japanese: Option<String>,
    pub season: Season,
    pub year: i32,
    pub platform: Platform,
    pub episode_offset: EpisodeOffset,
    pub mikan_id: Option<String>,
    pub bgmtv_id: Option<i64>,
}

/// Error when creating a Metadata entity.
#[derive(Debug, Error)]
pub enum CreateMetadataError {
    /// Chinese title is empty.
    #[error("Chinese title cannot be empty")]
    EmptyChineseTitle,
    /// Japanese title is empty (when provided).
    #[error("Japanese title cannot be empty")]
    EmptyJapaneseTitle,
    /// Invalid season number.
    #[error("Invalid season: {0}")]
    InvalidSeason(#[from] SeasonError),
}

impl CreateMetadataRequest {
    /// Create a new request with validation.
    pub fn new(
        title_chinese: &str,
        title_japanese: Option<&str>,
        season: i32,
        year: i32,
        platform: Platform,
    ) -> Result<Self, CreateMetadataError> {
        let title_chinese = title_chinese.trim();
        if title_chinese.is_empty() {
            return Err(CreateMetadataError::EmptyChineseTitle);
        }

        let title_japanese = match title_japanese {
            Some(t) => {
                let trimmed = t.trim();
                if trimmed.is_empty() {
                    return Err(CreateMetadataError::EmptyJapaneseTitle);
                }
                Some(trimmed.to_string())
            }
            None => None,
        };

        let season = Season::new(season)?;

        Ok(Self {
            title_chinese: title_chinese.to_string(),
            title_japanese,
            season,
            year,
            platform,
            episode_offset: EpisodeOffset::default(),
            mikan_id: None,
            bgmtv_id: None,
        })
    }

    /// Set the Mikan ID.
    pub fn with_mikan_id(mut self, mikan_id: String) -> Self {
        self.mikan_id = Some(mikan_id);
        self
    }

    /// Set the BGM.tv ID.
    pub fn with_bgmtv_id(mut self, bgmtv_id: i64) -> Self {
        self.bgmtv_id = Some(bgmtv_id);
        self
    }

    /// Set the episode offset.
    pub fn with_episode_offset(mut self, offset: EpisodeOffset) -> Self {
        self.episode_offset = offset;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_metadata_request_valid() {
        let request =
            CreateMetadataRequest::new("我推的孩子", Some("推しの子"), 2, 2024, Platform::Tv);
        assert!(request.is_ok());
        let request = request.unwrap();
        assert_eq!(request.title_chinese, "我推的孩子");
        assert_eq!(request.season.as_i32(), 2);
    }

    #[test]
    fn test_create_metadata_request_empty_title() {
        let request = CreateMetadataRequest::new("", None, 1, 2024, Platform::Tv);
        assert!(matches!(
            request,
            Err(CreateMetadataError::EmptyChineseTitle)
        ));
    }

    #[test]
    fn test_create_metadata_request_invalid_season() {
        let request = CreateMetadataRequest::new("Test", None, 0, 2024, Platform::Tv);
        assert!(matches!(
            request,
            Err(CreateMetadataError::InvalidSeason(_))
        ));
    }
}
