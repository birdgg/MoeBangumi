//! Bangumi creation request.

use thiserror::Error;

use super::SourceType;

/// Request to create a new Bangumi entity.
#[derive(Debug, Clone)]
pub struct CreateBangumiRequest {
    pub metadata_id: i64,
    pub save_path: String,
    pub source_type: SourceType,
}

/// Error when creating a Bangumi entity.
#[derive(Debug, Error)]
pub enum CreateBangumiError {
    /// Save path cannot be empty.
    #[error("Save path cannot be empty")]
    EmptySavePath,
}

impl CreateBangumiRequest {
    /// Create a new request with validation.
    pub fn new(
        metadata_id: i64,
        save_path: &str,
        source_type: SourceType,
    ) -> Result<Self, CreateBangumiError> {
        let save_path = save_path.trim();
        if save_path.is_empty() {
            return Err(CreateBangumiError::EmptySavePath);
        }

        Ok(Self {
            metadata_id,
            save_path: save_path.to_string(),
            source_type,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_bangumi_request_valid() {
        let request = CreateBangumiRequest::new(1, "/downloads/anime", SourceType::WebRip);
        assert!(request.is_ok());
    }

    #[test]
    fn test_create_bangumi_request_empty_path() {
        let request = CreateBangumiRequest::new(1, "  ", SourceType::WebRip);
        assert!(matches!(request, Err(CreateBangumiError::EmptySavePath)));
    }
}
