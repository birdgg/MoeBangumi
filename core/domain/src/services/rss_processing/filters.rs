//! RSS filtering utilities.
//!
//! This module contains functions for filtering RSS items based on
//! include/exclude patterns, publication date, and source type detection.

use regex::Regex;
use rss::{RssItem, RssSource};

/// Parse RSS URL to determine source type
pub(crate) fn parse_rss_source(url: &str) -> RssSource {
    let url_lower = url.to_lowercase();

    if url_lower.contains("nyaa.si") {
        RssSource::Nyaa(url.to_string())
    } else {
        RssSource::Mikan(url.to_string())
    }
}

/// Compile filter strings into regex patterns (case-insensitive)
fn compile_filters(filters: &[String]) -> Vec<Regex> {
    filters
        .iter()
        .filter_map(|pattern| {
            regex::RegexBuilder::new(pattern)
                .case_insensitive(true)
                .build()
                .map_err(|e| {
                    tracing::warn!("Invalid filter regex '{}': {}", pattern, e);
                    e
                })
                .ok()
        })
        .collect()
}

/// Check if title matches any of the filters (OR logic)
fn matches_any_filter(title: &str, filters: &[Regex]) -> bool {
    filters.iter().any(|re| re.is_match(title))
}

/// Filter RSS items by include and exclude filters
pub(crate) fn filter_rss_items(
    items: Vec<RssItem>,
    include_patterns: &[String],
    exclude_patterns: &[String],
) -> Vec<RssItem> {
    let include_filters = compile_filters(include_patterns);
    let exclude_filters = compile_filters(exclude_patterns);

    items
        .into_iter()
        .filter(|item| {
            let title = item.title();
            let include_ok =
                include_filters.is_empty() || matches_any_filter(title, &include_filters);
            let exclude_ok = !matches_any_filter(title, &exclude_filters);

            include_ok && exclude_ok
        })
        .collect()
}

/// Filter RSS items by pubDate, keeping only items newer than last_pub_date
pub(crate) fn filter_by_pub_date(items: Vec<RssItem>, last_pub_date: Option<&str>) -> Vec<RssItem> {
    let Some(last_pub_date) = last_pub_date else {
        return items;
    };

    let before_count = items.len();
    let filtered: Vec<_> = items
        .into_iter()
        .filter(|item| match &item.pub_date {
            Some(pub_date) => pub_date.as_str() > last_pub_date,
            None => true,
        })
        .collect();

    let filtered_count = before_count - filtered.len();
    if filtered_count > 0 {
        tracing::debug!(
            "Filtered {} items older than last_pub_date {}",
            filtered_count,
            last_pub_date
        );
    }

    filtered
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_rss_source_mikan() {
        let source = parse_rss_source("https://mikanani.me/RSS/Bangumi?bangumiId=3376");
        assert!(matches!(source, RssSource::Mikan(_)));
    }

    #[test]
    fn test_filter_by_pub_date_none() {
        let items = vec![
            create_test_item("Item 1", Some("2024-01-02")),
            create_test_item("Item 2", Some("2024-01-01")),
        ];
        let filtered = filter_by_pub_date(items, None);
        assert_eq!(filtered.len(), 2);
    }

    #[test]
    fn test_filter_by_pub_date_some() {
        let items = vec![
            create_test_item("Item 1", Some("2024-01-03")),
            create_test_item("Item 2", Some("2024-01-01")),
        ];
        let filtered = filter_by_pub_date(items, Some("2024-01-02"));
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].title(), "Item 1");
    }

    #[test]
    fn test_filter_rss_items_include() {
        let items = vec![
            create_test_item("ANi Episode 1", None),
            create_test_item("Other Group Episode 1", None),
        ];
        let filtered = filter_rss_items(items, &["ANi".to_string()], &[]);
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].title(), "ANi Episode 1");
    }

    #[test]
    fn test_filter_rss_items_exclude() {
        let items = vec![
            create_test_item("Episode 1 1080p", None),
            create_test_item("Episode 2 720p", None),
        ];
        let filtered = filter_rss_items(items, &[], &["720p".to_string()]);
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].title(), "Episode 1 1080p");
    }

    fn create_test_item(title: &str, pub_date: Option<&str>) -> RssItem {
        RssItem {
            title: title.to_string(),
            torrent_url: "http://example.com/torrent".to_string(),
            info_hash: "abc123".to_string(),
            pub_date: pub_date.map(|s| s.to_string()),
        }
    }
}
