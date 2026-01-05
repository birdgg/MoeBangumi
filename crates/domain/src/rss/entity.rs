//! RSS subscription entity.
//!
//! Represents an RSS feed subscription for tracking anime releases.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::CreateRssRequest;

/// RSS subscription entity.
///
/// Represents a subscription to an RSS feed for tracking anime releases.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rss {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,

    /// Foreign key to bangumi.
    pub bangumi_id: i64,
    /// RSS subscription title: [group] {bangumi} S{season}
    pub title: String,
    /// RSS feed URL.
    pub url: String,
    /// Whether subscription is enabled.
    pub enabled: bool,
    /// Regex patterns to exclude from matching.
    pub exclude_filters: Vec<String>,
    /// Regex patterns to include in matching.
    pub include_filters: Vec<String>,
    /// Optional subtitle group name.
    pub subtitle_group: Option<String>,

    /// HTTP caching: ETag from last response.
    pub etag: Option<String>,
    /// HTTP caching: Last-Modified from last response.
    pub last_modified: Option<String>,
    /// Last processed pubDate (ISO 8601).
    pub last_pub_date: Option<String>,
}

impl Rss {
    /// Create a new RSS subscription from a validated request.
    pub fn create(request: CreateRssRequest) -> Self {
        let now = Utc::now();
        Self {
            id: 0,
            created_at: now,
            updated_at: now,
            bangumi_id: request.bangumi_id,
            title: request.title,
            url: request.url,
            enabled: true,
            exclude_filters: request.exclude_filters,
            include_filters: request.include_filters,
            subtitle_group: request.subtitle_group,
            etag: None,
            last_modified: None,
            last_pub_date: None,
        }
    }

    /// Reconstitute an RSS from persistence (used by repository).
    #[allow(clippy::too_many_arguments)]
    pub fn reconstitute(
        id: i64,
        created_at: DateTime<Utc>,
        updated_at: DateTime<Utc>,
        bangumi_id: i64,
        title: String,
        url: String,
        enabled: bool,
        exclude_filters: Vec<String>,
        include_filters: Vec<String>,
        subtitle_group: Option<String>,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) -> Self {
        Self {
            id,
            created_at,
            updated_at,
            bangumi_id,
            title,
            url,
            enabled,
            exclude_filters,
            include_filters,
            subtitle_group,
            etag,
            last_modified,
            last_pub_date,
        }
    }

    // === Business Logic ===

    /// Update HTTP caching information.
    pub fn update_cache_info(
        &mut self,
        etag: Option<String>,
        last_modified: Option<String>,
        last_pub_date: Option<String>,
    ) {
        self.etag = etag;
        self.last_modified = last_modified;
        self.last_pub_date = last_pub_date;
        self.updated_at = Utc::now();
    }

    /// Check if this RSS subscription is active (enabled).
    pub fn is_active(&self) -> bool {
        self.enabled
    }
}

/// Format RSS subscription title.
///
/// Format: `[subtitle_group] {bangumi_title} S{season:02}` or `{bangumi_title} S{season:02}` if subtitle_group is None
pub fn format_rss_title(bangumi_title: &str, season: i32, subtitle_group: Option<&str>) -> String {
    match subtitle_group {
        Some(g) => format!("[{}] {} S{:02}", g, bangumi_title, season),
        None => format!("{} S{:02}", bangumi_title, season),
    }
}
