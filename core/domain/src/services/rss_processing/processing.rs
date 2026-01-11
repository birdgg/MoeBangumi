//! Single RSS processing logic.
//!
//! This module contains the implementation of `process_single` and related
//! private methods for processing a single RSS subscription.
//!
//! Processing flow:
//! 1. Fetch and filter RSS items (pubDate, include/exclude patterns)
//! 2. Build lookup from existing database torrents
//! 3. Parse items and analyze with unified decision-making
//! 4. Execute all actions in batch (add downloads, wash episodes)

use std::collections::HashMap;

use downloader::AddTaskOptions;
use parser::ParseResult;
use rss::{FetchContext, RssItem};
use washing::{ComparableTorrent, PriorityCalculator};

use crate::models::{CreateTorrent, Rss};
use crate::repositories::{BangumiRepository, RssRepository, TorrentRepository};
use crate::services::washing::WashParams;

use super::filters::{filter_by_pub_date, filter_rss_items, parse_rss_source};
use super::service::RssProcessingService;
use super::types::{AnalysisResult, FetchResult, PendingAction, ProcessingContext, TorrentLookup};

impl RssProcessingService {
    /// Process a single RSS subscription synchronously.
    ///
    /// This method fetches the RSS feed, analyzes items with unified decision-making,
    /// then executes all actions in batch. Errors are logged internally.
    pub async fn process_single(&self, rss: &Rss, global_exclude_filters: &[String]) {
        tracing::debug!("Processing RSS: {}", rss.title);

        // 1. Prepare processing context (get bangumi info)
        let Some(ctx) = self.prepare_context(rss).await else {
            return;
        };

        // 2. Fetch and filter RSS items (no parsing yet)
        let Some(fetch_result) = self
            .fetch_and_filter_items(rss, global_exclude_filters)
            .await
        else {
            return;
        };

        // 3. Build lookup structures for existing torrents
        let Some(lookup) = self.build_torrent_lookup(rss).await else {
            return;
        };

        // 4. Parse and analyze items with unified decision-making
        let result = self.analyze_items(&ctx, fetch_result.items, &lookup);

        // 5. Execute all actions in batch
        self.execute_actions(rss, &ctx, result).await;

        // 6. Update last_pub_date after successful processing
        if let Some(latest_pub_date) = fetch_result.latest_pub_date {
            if let Err(e) =
                RssRepository::update_cache(&self.db, rss.id, None, None, Some(latest_pub_date))
                    .await
            {
                tracing::warn!("[{}] Failed to update last_pub_date: {}", rss.title, e);
            }
        }
    }

    /// Prepare processing context by fetching bangumi info
    async fn prepare_context(&self, rss: &Rss) -> Option<ProcessingContext> {
        let bangumi = match BangumiRepository::get_by_id(&self.db, rss.bangumi_id).await {
            Ok(Some(b)) => b,
            Ok(None) => {
                tracing::error!("[bangumi_id={}] Bangumi not found", rss.bangumi_id);
                return None;
            }
            Err(e) => {
                tracing::error!(
                    "[bangumi_id={}] Failed to get bangumi: {}",
                    rss.bangumi_id,
                    e
                );
                return None;
            }
        };

        Some(ProcessingContext { bangumi })
    }

    /// Fetch RSS feed and apply filters (pubDate, include/exclude patterns).
    /// Does NOT parse items - that's done in analyze_items.
    ///
    /// Updates etag/last_modified immediately after fetch, but returns latest_pub_date
    /// for the caller to update after successful processing.
    async fn fetch_and_filter_items(
        &self,
        rss: &Rss,
        global_exclude_filters: &[String],
    ) -> Option<FetchResult> {
        // Parse URL to determine source type
        let source = parse_rss_source(&rss.url);

        // Build fetch context from stored cache info
        let fetch_context = FetchContext {
            etag: rss.etag.clone(),
            last_modified: rss.last_modified.clone(),
        };

        // Fetch RSS feed with conditional request (ETag/Last-Modified)
        let rss_fetch_result = match self
            .rss_client
            .fetch_conditional(&source, Some(&fetch_context))
            .await
        {
            Ok(result) => result,
            Err(e) => {
                tracing::error!("[{}] RSS fetch failed: {}", rss.title, e);
                return None;
            }
        };

        // Handle fetch result
        let (items, new_etag, new_last_modified) = match rss_fetch_result {
            rss::FetchResult::NotModified => {
                tracing::debug!("[{}] RSS not modified (HTTP 304), skipping", rss.title);
                return None;
            }
            rss::FetchResult::Modified {
                items,
                etag,
                last_modified,
            } => (items, etag, last_modified),
        };

        // Extract latest pub_date from ALL items BEFORE filtering (for cache update after processing)
        let latest_pub_date = items
            .iter()
            .filter_map(|item| item.pub_date.as_ref())
            .max()
            .cloned()
            .or_else(|| rss.last_pub_date.clone());

        // Update etag/last_modified immediately (HTTP cache)
        // last_pub_date will be updated after successful processing
        if let Err(e) =
            RssRepository::update_cache(&self.db, rss.id, new_etag, new_last_modified, None).await
        {
            tracing::warn!("[{}] Failed to update cache info: {}", rss.title, e);
        }

        // Apply pubDate filter (only process items newer than last_pub_date)
        let items = filter_by_pub_date(items, rss.last_pub_date.as_deref());

        // Merge global and RSS-specific exclude filters
        let all_exclude_filters: Vec<String> = global_exclude_filters
            .iter()
            .chain(rss.exclude_filters.iter())
            .cloned()
            .collect();

        // Filter items by include/exclude patterns
        let filtered_items = filter_rss_items(items, &rss.include_filters, &all_exclude_filters);

        Some(FetchResult {
            items: filtered_items,
            latest_pub_date,
        })
    }

    /// Build lookup structures for existing torrents (batch fetch to avoid N+1).
    ///
    /// Returns a `TorrentLookup` containing two maps for O(1) access:
    /// - `existing_hashes`: HashSet of info_hash strings to quickly check if a torrent
    ///   already exists in the database (used to skip duplicate downloads)
    /// - `episodes_map`: HashMap from episode number to Vec<Torrent>, used to find
    ///   existing torrents for an episode when deciding whether to "wash" (replace
    ///   with higher priority torrent)
    async fn build_torrent_lookup(&self, rss: &Rss) -> Option<TorrentLookup> {
        let existing_torrents =
            match TorrentRepository::get_by_bangumi_id(&self.db, rss.bangumi_id).await {
                Ok(torrents) => torrents,
                Err(e) => {
                    tracing::error!("[{}] Failed to fetch existing torrents: {}", rss.title, e);
                    return None;
                }
            };

        Some(TorrentLookup::from_torrents(existing_torrents))
    }

    /// Parse items and analyze with unified decision-making.
    ///
    /// This method:
    /// 1. Parses all items to extract episode numbers and metadata
    /// 2. Groups items by episode
    /// 3. For each episode, considers both:
    ///    - All candidates from this RSS feed
    ///    - Existing torrents in database
    /// 4. Decides the best action: skip, add, or wash
    fn analyze_items(
        &self,
        ctx: &ProcessingContext,
        items: Vec<RssItem>,
        lookup: &TorrentLookup,
    ) -> AnalysisResult {
        // Build priority calculator from current settings
        let settings = self.settings.get();
        let priority_config = settings.priority.to_config();
        let calculator = PriorityCalculator::new(priority_config);

        // Parse all items and group by episode
        let mut episodes_candidates: HashMap<i32, Vec<(RssItem, ParseResult)>> = HashMap::new();

        for item in items {
            let title = item.title();

            // Skip if already exists in database
            if lookup.existing_hashes.contains(item.info_hash()) {
                tracing::debug!("Skipping existing torrent: {}", title);
                continue;
            }

            // Parse title
            let parse_result = match self.parser.parse(title) {
                Ok(result) => result,
                Err(e) => {
                    tracing::warn!("Failed to parse title '{}': {}", title, e);
                    continue;
                }
            };

            // Skip if no episode number
            let Some(episode) = parse_result.episode else {
                continue;
            };

            episodes_candidates
                .entry(episode)
                .or_default()
                .push((item, parse_result));
        }

        // When auto_complete is disabled, only keep the latest episode
        let episodes_candidates = if !ctx.bangumi.auto_complete {
            if let Some(&max_ep) = episodes_candidates.keys().max() {
                tracing::debug!(
                    "auto_complete disabled for bangumi {}: only processing latest episode {}",
                    ctx.bangumi.id,
                    max_ep
                );
                episodes_candidates
                    .into_iter()
                    .filter(|(ep, _)| *ep == max_ep)
                    .collect()
            } else {
                episodes_candidates
            }
        } else {
            episodes_candidates
        };

        // Analyze each episode and decide action
        let mut actions = Vec::new();

        for (episode, candidates) in episodes_candidates {
            // Find best candidate from RSS feed
            // Note: min_by_key because lower score = higher priority
            let best_candidate = candidates.into_iter().min_by_key(|(_, parse_result)| {
                let comparable = ComparableTorrent {
                    subtitle_group: parse_result.subtitle_group.clone(),
                    subtitle_languages: parse_result.subtitle_language.clone(),
                };
                calculator.calculate_score(&comparable)
            });

            let Some((best_item, best_parse_result)) = best_candidate else {
                continue;
            };

            // Check if episode exists in database
            if let Some(existing_torrents) = lookup.episodes_map.get(&episode) {
                // Compare with existing best
                if self
                    .washing
                    .should_wash(existing_torrents, &best_parse_result)
                {
                    // Wash: new candidate is better
                    tracing::debug!(
                        "E{}: will wash (group={:?}, lang={:?})",
                        episode,
                        best_parse_result.subtitle_group,
                        best_parse_result.subtitle_language,
                    );
                    actions.push(PendingAction::Wash {
                        item: best_item,
                        episode,
                        parse_result: best_parse_result,
                        existing_torrents: existing_torrents.clone(),
                    });
                } else {
                    tracing::debug!(
                        "E{}: skipping, existing torrent has higher or equal priority",
                        episode
                    );
                }
            } else {
                // No existing torrents, add new
                tracing::debug!(
                    "E{}: will add (group={:?}, lang={:?})",
                    episode,
                    best_parse_result.subtitle_group,
                    best_parse_result.subtitle_language,
                );
                actions.push(PendingAction::Add {
                    item: best_item,
                    episode,
                    parse_result: best_parse_result,
                });
            }
        }

        AnalysisResult { actions }
    }

    /// Generate filename for an episode based on bangumi info.
    fn generate_episode_filename(&self, ctx: &ProcessingContext, episode: i32) -> String {
        let adjusted_episode = ctx.bangumi.adjust_episode(episode);
        pathgen::generate_filename(
            &ctx.bangumi.title_chinese,
            ctx.bangumi.season,
            adjusted_episode,
            Some(ctx.bangumi.platform.as_str()),
        )
    }

    /// Execute all pending actions in batch.
    async fn execute_actions(&self, rss: &Rss, ctx: &ProcessingContext, result: AnalysisResult) {
        if result.actions.is_empty() {
            return;
        }

        tracing::debug!("[{}] Executing {} actions", rss.title, result.actions.len());

        for action in result.actions {
            match action {
                PendingAction::Add {
                    item,
                    episode,
                    parse_result,
                } => {
                    self.execute_add(rss, ctx, &item, episode, &parse_result)
                        .await;
                }
                PendingAction::Wash {
                    item,
                    episode,
                    parse_result,
                    existing_torrents,
                } => {
                    self.execute_wash(rss, ctx, &item, episode, &parse_result, &existing_torrents)
                        .await;
                }
            }
        }
    }

    /// Execute add action: create torrent record and queue download task
    async fn execute_add(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        item: &RssItem,
        episode: i32,
        parse_result: &ParseResult,
    ) {
        let info_hash = item.info_hash();
        let torrent_url = item.torrent_url();
        let filename = self.generate_episode_filename(ctx, episode);

        // 1. Create torrent record in database
        let torrent = CreateTorrent {
            bangumi_id: Some(rss.bangumi_id),
            rss_id: Some(rss.id),
            info_hash: info_hash.to_string(),
            torrent_url: torrent_url.to_string(),
            episode_number: Some(episode),
            subtitle_group: parse_result.subtitle_group.clone(),
            subtitle_languages: parse_result.subtitle_language.clone(),
            resolution: parse_result.resolution.clone(),
        };

        if let Err(e) = TorrentRepository::create_with_executor(&self.db, torrent).await {
            tracing::error!("[{}] Failed to create torrent record: {}", rss.title, e);
            return;
        }

        // 2. Add download task
        let options = AddTaskOptions::new(torrent_url)
            .save_path(&ctx.bangumi.save_path)
            .rename(&filename);

        if let Err(e) = self.downloader.add_task(options).await {
            tracing::error!("[{}] Failed to add download task: {}", rss.title, e);
            return;
        }

        tracing::debug!(
            "[{}] Queued download for E{}: {}",
            rss.title,
            episode,
            info_hash
        );
    }

    /// Execute wash action: replace existing torrents with higher priority one
    async fn execute_wash(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        item: &RssItem,
        episode: i32,
        parse_result: &ParseResult,
        existing_torrents: &[crate::models::Torrent],
    ) {
        let info_hash = item.info_hash();
        let torrent_url = item.torrent_url();
        let filename = self.generate_episode_filename(ctx, episode);

        let params = WashParams {
            bangumi_id: rss.bangumi_id,
            rss_id: Some(rss.id),
            rss_title: &rss.title,
            existing_torrents,
            info_hash,
            torrent_url,
            episode,
            parse_result,
            save_path: &ctx.bangumi.save_path,
            rename: &filename,
        };

        if let Err(e) = self.washing.wash_episode(params).await {
            tracing::error!("[{}] Failed to wash E{}: {}", rss.title, episode, e);
        }
    }
}

#[cfg(test)]
mod tests {
    use parser::{Parser, SubType};
    use rss::RssItem;
    use washing::{ComparableTorrent, PriorityCalculator, PriorityConfig, SubtitleLanguageSet};

    use crate::models::Torrent;

    use super::TorrentLookup;

    /// Helper to create a test RssItem
    fn create_rss_item(title: &str, info_hash: &str) -> RssItem {
        RssItem {
            title: title.to_string(),
            torrent_url: format!("http://example.com/{}.torrent", info_hash),
            info_hash: info_hash.to_string(),
            pub_date: Some("2025-01-01T00:00:00Z".to_string()),
        }
    }

    /// Helper to create a test Torrent
    fn create_existing_torrent(
        episode: i32,
        info_hash: &str,
        subtitle_group: Option<&str>,
        subtitle_languages: Vec<SubType>,
    ) -> Torrent {
        Torrent {
            id: 1,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
            bangumi_id: Some(1),
            rss_id: Some(1),
            info_hash: info_hash.to_string(),
            torrent_url: format!("http://example.com/{}.torrent", info_hash),
            episode_number: Some(episode),
            subtitle_group: subtitle_group.map(|s| s.to_string()),
            subtitle_languages,
            resolution: Some("1080p".to_string()),
        }
    }

    /// Test that parser correctly extracts subtitle group and language from titles
    #[test]
    fn test_parse_washing_candidates() {
        let parser = Parser::new();

        // Title 1: 萌樱字幕组, 简日双语
        let title1 = "[萌樱字幕组][简日双语][间谍过家家][第三季][13][Webrip][1080p][简繁日内封]";
        let result1 = parser.parse(title1).expect("Failed to parse title1");

        assert_eq!(result1.subtitle_group, Some("萌樱字幕组".to_string()));
        assert_eq!(result1.episode, Some(13));
        // 简日双语 should parse as [Chs, Jpn]
        assert!(
            result1.subtitle_language.contains(&SubType::Chs),
            "Expected Chs in {:?}",
            result1.subtitle_language
        );
        assert!(
            result1.subtitle_language.contains(&SubType::Jpn),
            "Expected Jpn in {:?}",
            result1.subtitle_language
        );

        // Title 2: 桜都字幕组, 繁体内嵌
        let title2 = "[桜都字幕组] 间谍家家酒 第三季 / Spy x Family (2025) [13][1080p][繁体内嵌]";
        let result2 = parser.parse(title2).expect("Failed to parse title2");

        assert_eq!(result2.subtitle_group, Some("桜都字幕组".to_string()));
        assert_eq!(result2.episode, Some(13));
        // 繁体内嵌 should parse as [Cht]
        assert!(
            result2.subtitle_language.contains(&SubType::Cht),
            "Expected Cht in {:?}",
            result2.subtitle_language
        );
    }

    /// Test that priority calculator correctly selects 桜都字幕组 as higher priority
    /// when configured with 桜都字幕组 as the top priority subtitle group.
    #[test]
    fn test_washing_selects_higher_priority_group() {
        let parser = Parser::new();

        // Parse both titles
        let title1 = "[萌樱字幕组][简日双语][间谍过家家][第三季][13][Webrip][1080p][简繁日内封]";
        let title2 = "[桜都字幕组] 间谍家家酒 第三季 / Spy x Family (2025) [13][1080p][繁体内嵌]";

        let result1 = parser.parse(title1).expect("Failed to parse title1");
        let result2 = parser.parse(title2).expect("Failed to parse title2");

        // Create priority config with 桜都字幕组 as highest priority
        let config = PriorityConfig {
            subtitle_groups: vec!["桜都字幕组".to_string(), "萌樱字幕组".to_string()],
            subtitle_language_sets: vec![
                SubtitleLanguageSet::new(vec![SubType::Cht]), // 繁体 highest
                SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Jpn]), // 简日
            ],
        };

        let calculator = PriorityCalculator::new(config);

        // Create comparable torrents
        let comparable1 = ComparableTorrent {
            subtitle_group: result1.subtitle_group.clone(),
            subtitle_languages: result1.subtitle_language.clone(),
        };
        let comparable2 = ComparableTorrent {
            subtitle_group: result2.subtitle_group.clone(),
            subtitle_languages: result2.subtitle_language.clone(),
        };

        // Calculate scores
        let score1 = calculator.calculate_score(&comparable1);
        let score2 = calculator.calculate_score(&comparable2);

        // 桜都字幕组 should have lower score (higher priority)
        // group_rank: 桜都=0, 萌樱=1
        assert_eq!(score2.group_rank, 0, "桜都字幕组 should have group_rank 0");
        assert_eq!(score1.group_rank, 1, "萌樱字幕组 should have group_rank 1");

        // Lower score = higher priority
        assert!(
            score2 < score1,
            "桜都字幕组 score ({:?}) should be less than 萌樱字幕组 score ({:?})",
            score2,
            score1
        );

        // is_higher_priority should confirm 桜都 > 萌樱
        assert!(
            calculator.is_higher_priority(&comparable2, &comparable1),
            "桜都字幕组 should have higher priority than 萌樱字幕组"
        );
    }

    /// Test the full washing scenario:
    /// - Existing torrent: 萌樱字幕组 episode 13
    /// - New RSS items: 萌樱字幕组 and 桜都字幕组 both for episode 13
    /// - Expected: 桜都字幕组 should be selected for washing
    #[test]
    fn test_washing_scenario_with_existing_torrent() {
        let parser = Parser::new();

        // Parse new RSS items
        let title1 = "[萌樱字幕组][简日双语][间谍过家家][第三季][13][Webrip][1080p][简繁日内封]";
        let title2 = "[桜都字幕组] 间谍家家酒 第三季 / Spy x Family (2025) [13][1080p][繁体内嵌]";

        let result1 = parser.parse(title1).expect("Failed to parse title1");
        let result2 = parser.parse(title2).expect("Failed to parse title2");

        // Create existing torrent (萌樱字幕组)
        let existing_torrent = create_existing_torrent(
            13,
            "existing_hash_001",
            Some("萌樱字幕组"),
            vec![SubType::Chs, SubType::Jpn],
        );

        // Build lookup with existing torrent
        let lookup = TorrentLookup::from_torrents(vec![existing_torrent.clone()]);

        // Verify lookup structure
        assert!(lookup.existing_hashes.contains("existing_hash_001"));
        assert!(lookup.episodes_map.contains_key(&13));
        assert_eq!(lookup.episodes_map.get(&13).unwrap().len(), 1);

        // Create priority config with 桜都字幕组 as highest priority
        let config = PriorityConfig {
            subtitle_groups: vec!["桜都字幕组".to_string(), "萌樱字幕组".to_string()],
            subtitle_language_sets: vec![
                SubtitleLanguageSet::new(vec![SubType::Cht]),
                SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Jpn]),
            ],
        };

        let calculator = PriorityCalculator::new(config);

        // Simulate analyze_items logic: find best candidate from RSS
        let candidates = vec![
            (create_rss_item(title1, "new_hash_001"), result1.clone()),
            (create_rss_item(title2, "new_hash_002"), result2.clone()),
        ];

        // Find best candidate (lowest score = highest priority)
        let best_candidate = candidates.into_iter().min_by_key(|(_, parse_result)| {
            let comparable = ComparableTorrent {
                subtitle_group: parse_result.subtitle_group.clone(),
                subtitle_languages: parse_result.subtitle_language.clone(),
            };
            calculator.calculate_score(&comparable)
        });

        let (best_item, best_parse_result) = best_candidate.expect("Should have a best candidate");

        // Verify 桜都字幕组 is selected as best
        assert_eq!(
            best_parse_result.subtitle_group,
            Some("桜都字幕组".to_string()),
            "Best candidate should be 桜都字幕组"
        );
        assert_eq!(best_item.info_hash, "new_hash_002");

        // Now check if we should wash: compare new best vs existing
        let new_comparable = ComparableTorrent {
            subtitle_group: best_parse_result.subtitle_group.clone(),
            subtitle_languages: best_parse_result.subtitle_language.clone(),
        };

        let existing_comparable = existing_torrent.to_comparable();

        // 桜都字幕组 should have higher priority than existing 萌樱字幕组
        assert!(
            calculator.is_higher_priority(&new_comparable, &existing_comparable),
            "New torrent (桜都字幕组) should have higher priority than existing (萌樱字幕组)"
        );
    }
}
