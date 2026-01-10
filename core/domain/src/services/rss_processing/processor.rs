//! Pure business logic for RSS processing.
//!
//! This module contains stateless functions that implement RSS processing logic
//! without any side effects (no database, no network).

use parser::{ParseResult, Parser};
use rss::RssItem;
use std::collections::{HashMap, HashSet};
use washing::{ComparableTorrent, PriorityCalculator, PriorityConfig};

use crate::models::{BangumiWithSeries, Torrent};

use super::filters::MAX_ITEMS_PER_RSS;
use super::traits::ProcessAction;

/// Pure RSS processing logic without side effects
pub struct RssProcessor {
    parser: Parser,
}

#[allow(dead_code)]
impl RssProcessor {
    pub fn new() -> Self {
        Self {
            parser: Parser::new(),
        }
    }

    /// Parse RSS items to extract episode numbers and metadata
    pub fn parse_items(&self, items: Vec<RssItem>) -> Vec<(RssItem, i32, ParseResult)> {
        items
            .into_iter()
            .filter_map(|item| {
                let title = item.title();
                match self.parser.parse(title) {
                    Ok(parse_result) => parse_result
                        .episode
                        .map(|ep| (item, ep, parse_result.clone())),
                    Err(e) => {
                        tracing::warn!("Failed to parse title '{}': {}", title, e);
                        None
                    }
                }
            })
            .collect()
    }

    /// Filter parsed items to keep only highest priority per episode
    pub fn filter_by_priority(
        &self,
        parsed_items: Vec<(RssItem, i32, ParseResult)>,
        priority_config: PriorityConfig,
    ) -> Vec<(RssItem, i32, ParseResult)> {
        if parsed_items.is_empty() {
            return parsed_items;
        }

        let calculator = PriorityCalculator::new(priority_config);

        // Group items by episode number
        let mut episodes_map: HashMap<i32, Vec<(RssItem, i32, ParseResult)>> = HashMap::new();
        for item in parsed_items {
            episodes_map.entry(item.1).or_default().push(item);
        }

        // For each episode, keep only the highest priority item
        let mut result = Vec::new();
        for (episode, items) in episodes_map {
            let item_count = items.len();

            let best_item = items.into_iter().min_by_key(|(_, _, parse_result)| {
                let comparable = ComparableTorrent {
                    subtitle_group: parse_result.subtitle_group.clone(),
                    subtitle_languages: parse_result.subtitle_language.clone(),
                };
                calculator.calculate_score(&comparable)
            });

            if let Some(item) = best_item {
                if item_count > 1 {
                    tracing::debug!(
                        "E{}: kept highest priority item (group={:?}, lang={:?}) from {} candidates",
                        episode,
                        item.2.subtitle_group,
                        item.2.subtitle_language,
                        item_count
                    );
                }
                result.push(item);
            }
        }

        result
    }

    /// Limit items to only the latest episode when auto_complete is disabled
    pub fn limit_to_latest_episode(
        &self,
        mut parsed_items: Vec<(RssItem, i32, ParseResult)>,
        auto_complete: bool,
        bangumi_id: i64,
    ) -> Vec<(RssItem, i32, ParseResult)> {
        if auto_complete {
            return parsed_items;
        }

        parsed_items.sort_by(|a, b| b.1.cmp(&a.1));
        if let Some((_, ep, _)) = parsed_items.first() {
            tracing::debug!(
                "auto_complete disabled for bangumi {}: only processing latest episode {}",
                bangumi_id,
                ep
            );
            parsed_items.truncate(1);
        }
        parsed_items
    }

    /// Truncate items to MAX_ITEMS_PER_RSS
    pub fn truncate_items(&self, mut items: Vec<RssItem>, rss_title: &str) -> Vec<RssItem> {
        if items.len() > MAX_ITEMS_PER_RSS {
            tracing::warn!(
                "[{}] RSS has {} items, limiting to {}",
                rss_title,
                items.len(),
                MAX_ITEMS_PER_RSS
            );
            items.truncate(MAX_ITEMS_PER_RSS);
        }
        items
    }

    /// Determine what action to take for a single RSS item
    pub fn determine_action(
        &self,
        item: &RssItem,
        episode: i32,
        parse_result: &ParseResult,
        existing_hashes: &HashSet<String>,
        episode_torrents: &HashMap<i32, Vec<(Torrent, ComparableTorrent)>>,
        priority_config: &PriorityConfig,
        bangumi: &BangumiWithSeries,
        save_path: &str,
        rss_id: Option<i64>,
        rss_title: &str,
    ) -> ProcessAction {
        let info_hash = item.info_hash();
        let torrent_url = item.torrent_url();

        // Skip if torrent already exists
        if existing_hashes.contains(info_hash) {
            return ProcessAction::Skip {
                reason: format!("Torrent {} already exists", info_hash),
            };
        }

        // Calculate adjusted episode and filename
        let adjusted_episode = bangumi.bangumi.adjust_episode(episode);
        let filename = pathgen::generate_filename(
            &bangumi.bangumi.title_chinese,
            bangumi.bangumi.season,
            adjusted_episode,
            Some(bangumi.bangumi.platform.as_str()),
        );

        // Check if there are existing torrents for this episode
        if let Some(existing) = episode_torrents.get(&episode) {
            if !existing.is_empty() {
                let calculator = PriorityCalculator::new(priority_config.clone());
                let new_comparable = ComparableTorrent {
                    subtitle_group: parse_result.subtitle_group.clone(),
                    subtitle_languages: parse_result.subtitle_language.clone(),
                };

                let existing_comparables: Vec<ComparableTorrent> =
                    existing.iter().map(|(_, c)| c.clone()).collect();

                if let Some(best_existing) = calculator.find_best(&existing_comparables) {
                    if calculator.is_higher_priority(&new_comparable, best_existing) {
                        let existing_ids: Vec<i64> = existing.iter().map(|(t, _)| t.id).collect();
                        let existing_hashes: Vec<String> =
                            existing.iter().map(|(t, _)| t.info_hash.clone()).collect();

                        return ProcessAction::Wash {
                            info_hash: info_hash.to_string(),
                            torrent_url: torrent_url.to_string(),
                            episode,
                            adjusted_episode,
                            filename,
                            save_path: save_path.to_string(),
                            bangumi_id: bangumi.bangumi.id,
                            rss_id,
                            existing_torrent_ids: existing_ids,
                            existing_info_hashes: existing_hashes,
                        };
                    }
                }

                return ProcessAction::Skip {
                    reason: format!(
                        "[{}] E{}: lower/equal priority (group={:?}, lang={:?})",
                        rss_title,
                        episode,
                        parse_result.subtitle_group,
                        parse_result.subtitle_language
                    ),
                };
            }
        }

        ProcessAction::Add {
            info_hash: info_hash.to_string(),
            torrent_url: torrent_url.to_string(),
            episode,
            adjusted_episode,
            filename,
            save_path: save_path.to_string(),
            bangumi_id: bangumi.bangumi.id,
            rss_id,
        }
    }

    /// Parse filename to get full ParseResult
    pub fn parse_filename(&self, filename: &str) -> Option<ParseResult> {
        self.parser.parse(filename).ok()
    }
}

impl Default for RssProcessor {
    fn default() -> Self {
        Self::new()
    }
}
