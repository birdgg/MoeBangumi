//! RSS subscription creation request.

use thiserror::Error;

use crate::metadata::Season;

/// Request to create a new RSS subscription.
#[derive(Debug, Clone)]
pub struct CreateRssRequest {
    pub bangumi_id: i64,
    pub title: String,
    pub url: String,
    pub exclude_filters: Vec<String>,
    pub include_filters: Vec<String>,
    pub subtitle_group: Option<String>,
}

/// Error when creating an RSS subscription.
#[derive(Debug, Error)]
pub enum CreateRssError {
    /// RSS URL cannot be empty.
    #[error("RSS URL cannot be empty")]
    EmptyUrl,
    /// RSS URL is invalid.
    #[error("Invalid RSS URL format")]
    InvalidUrl,
}

impl CreateRssRequest {
    /// Create a new request with validation.
    pub fn new(
        bangumi_id: i64,
        bangumi_title: &str,
        season: Season,
        url: &str,
        subtitle_group: Option<&str>,
    ) -> Result<Self, CreateRssError> {
        let url = url.trim();
        if url.is_empty() {
            return Err(CreateRssError::EmptyUrl);
        }

        // Basic URL validation
        if !url.starts_with("http://") && !url.starts_with("https://") {
            return Err(CreateRssError::InvalidUrl);
        }

        // Format the RSS title
        let title = match subtitle_group {
            Some(g) => format!("[{}] {} S{:02}", g, bangumi_title, season.as_i32()),
            None => format!("{} S{:02}", bangumi_title, season.as_i32()),
        };

        Ok(Self {
            bangumi_id,
            title,
            url: url.to_string(),
            exclude_filters: Vec::new(),
            include_filters: Vec::new(),
            subtitle_group: subtitle_group.map(|s| s.to_string()),
        })
    }

    /// Add exclude filters.
    pub fn with_exclude_filters(mut self, filters: Vec<String>) -> Self {
        self.exclude_filters = filters;
        self
    }

    /// Add include filters.
    pub fn with_include_filters(mut self, filters: Vec<String>) -> Self {
        self.include_filters = filters;
        self
    }
}
