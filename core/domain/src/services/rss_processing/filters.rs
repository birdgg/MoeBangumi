//! RSS item filtering functions.

use regex::Regex;
use rss::{RssItem, RssSource};

/// Maximum number of items to process per RSS feed
pub const MAX_ITEMS_PER_RSS: usize = 200;

/// Parse RSS URL to determine source type
pub fn parse_rss_source(url: &str) -> RssSource {
    let url_lower = url.to_lowercase();

    if url_lower.contains("nyaa.si") {
        RssSource::Nyaa(url.to_string())
    } else {
        // Default to Mikan (covers mikanani.me and other sources)
        RssSource::Mikan(url.to_string())
    }
}

/// Compile filter strings into regex patterns (case-insensitive)
pub fn compile_filters(filters: &[String]) -> Vec<Regex> {
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
pub fn matches_any_filter(title: &str, filters: &[Regex]) -> bool {
    filters.iter().any(|re| re.is_match(title))
}

/// Filter RSS items by include and exclude filters
///
/// Filtering logic (both conditions must be satisfied):
/// 1. Include filter: If include_patterns is empty, all items pass;
///    otherwise item must match at least one include pattern (OR logic)
/// 2. Exclude filter: Item must NOT match any exclude pattern
///
/// Returns items that satisfy BOTH conditions (AND logic)
pub fn filter_rss_items(
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
            // Must match ANY include filter (if include is not empty)
            let include_ok =
                include_filters.is_empty() || matches_any_filter(title, &include_filters);
            // Must NOT match ANY exclude filter
            let exclude_ok = !matches_any_filter(title, &exclude_filters);

            include_ok && exclude_ok
        })
        .collect()
}

/// Filter RSS items by pubDate, keeping only items newer than last_pub_date
/// Items without pub_date are always included (conservative approach)
pub fn filter_by_pub_date(items: Vec<RssItem>, last_pub_date: Option<&str>) -> Vec<RssItem> {
    let Some(last_pub_date) = last_pub_date else {
        // No previous pub_date stored, include all items
        return items;
    };

    let before_count = items.len();
    let filtered: Vec<_> = items
        .into_iter()
        .filter(|item| {
            match &item.pub_date {
                Some(pub_date) => pub_date.as_str() > last_pub_date,
                // Items without pub_date are included (conservative)
                None => true,
            }
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

    fn create_item(title: &str, info_hash: &str) -> RssItem {
        RssItem {
            title: title.to_string(),
            torrent_url: format!("https://example.com/{}.torrent", info_hash),
            info_hash: info_hash.to_string(),
            pub_date: None,
        }
    }

    fn create_item_with_date(title: &str, info_hash: &str, pub_date: Option<&str>) -> RssItem {
        RssItem {
            title: title.to_string(),
            torrent_url: format!("https://example.com/{}.torrent", info_hash),
            info_hash: info_hash.to_string(),
            pub_date: pub_date.map(|s| s.to_string()),
        }
    }

    #[test]
    fn test_filter_rss_items() {
        // No filters - pass all
        let items = vec![create_item("A", "1"), create_item("B", "2")];
        assert_eq!(filter_rss_items(items, &[], &[]).len(), 2);

        // Include filter
        let items = vec![
            create_item("[Lilith] A", "1"),
            create_item("[Other] B", "2"),
        ];
        let result = filter_rss_items(items, &["Lilith".to_string()], &[]);
        assert_eq!(result.len(), 1);
        assert!(result[0].title().contains("Lilith"));

        // Exclude filter
        let items = vec![create_item("A [1080p]", "1"), create_item("B [720p]", "2")];
        let result = filter_rss_items(items, &[], &["720p".to_string()]);
        assert_eq!(result.len(), 1);
        assert!(result[0].title().contains("1080p"));

        // Case insensitive
        let items = vec![create_item("[LILITH] A", "1")];
        let result = filter_rss_items(items, &["lilith".to_string()], &[]);
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_filter_by_pub_date() {
        // No last_pub_date - include all
        let items = vec![
            create_item_with_date("A", "1", Some("2024-01-01")),
            create_item_with_date("B", "2", Some("2024-01-02")),
        ];
        assert_eq!(filter_by_pub_date(items, None).len(), 2);

        // Filter old items
        let items = vec![
            create_item_with_date("A", "1", Some("2024-01-01")),
            create_item_with_date("B", "2", Some("2024-01-03")),
        ];
        let result = filter_by_pub_date(items, Some("2024-01-02"));
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].pub_date.as_deref(), Some("2024-01-03"));

        // Keep items without pub_date (conservative)
        let items = vec![
            create_item_with_date("A", "1", None),
            create_item_with_date("B", "2", Some("2024-01-01")),
        ];
        let result = filter_by_pub_date(items, Some("2024-01-02"));
        assert_eq!(result.len(), 1);
        assert!(result[0].pub_date.is_none());
    }

    #[test]
    fn test_compile_filters() {
        let filters = vec!["test".to_string(), "\\d+".to_string()];
        assert_eq!(compile_filters(&filters).len(), 2);

        // Invalid regex is skipped
        let filters = vec!["valid".to_string(), "[invalid".to_string()];
        assert_eq!(compile_filters(&filters).len(), 1);
    }
}
