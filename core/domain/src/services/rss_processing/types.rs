//! Internal types for RSS processing.
//!
//! This module contains internal data structures used during RSS processing.

use std::collections::{HashMap, HashSet};

use parser::ParseResult;
use rss::RssItem;

use crate::models::{Bangumi, Torrent};

/// Context for RSS processing (avoids repeated parameter passing)
pub(crate) struct ProcessingContext {
    pub bangumi: Bangumi,
}

/// Lookup structures for existing torrents, enabling O(1) access patterns.
///
/// Built once per RSS processing cycle to avoid N+1 database queries.
pub(crate) struct TorrentLookup {
    /// Set of info_hash strings for deduplication.
    /// Used to skip RSS items that already exist in the database.
    pub existing_hashes: HashSet<String>,

    /// Map from episode number to existing torrents for that episode.
    /// Used to determine if a new torrent should "wash" (replace) existing ones
    /// based on priority comparison (subtitle group, language, etc.).
    pub episodes_map: HashMap<i32, Vec<Torrent>>,
}

impl TorrentLookup {
    pub fn from_torrents(torrents: Vec<Torrent>) -> Self {
        let mut existing_hashes = HashSet::new();
        let mut episodes_map: HashMap<i32, Vec<Torrent>> = HashMap::new();

        for torrent in torrents {
            existing_hashes.insert(torrent.info_hash.clone());
            if let Some(ep) = torrent.episode_number {
                episodes_map.entry(ep).or_default().push(torrent);
            }
        }

        Self {
            existing_hashes,
            episodes_map,
        }
    }
}

/// Pending action to be executed after analysis
pub(crate) enum PendingAction {
    /// Add a new download (episode doesn't exist in database)
    Add {
        item: RssItem,
        episode: i32,
        parse_result: ParseResult,
    },
    /// Wash: replace existing torrents with higher priority one
    Wash {
        item: RssItem,
        episode: i32,
        parse_result: ParseResult,
        existing_torrents: Vec<Torrent>,
    },
}

/// Result of analyzing RSS items
pub(crate) struct AnalysisResult {
    /// Actions to execute
    pub actions: Vec<PendingAction>,
}

/// Result of fetching and filtering RSS items
pub(crate) struct FetchResult {
    /// Filtered items ready for analysis
    pub items: Vec<RssItem>,
    /// Latest pub_date from feed (before filtering), for cache update after successful processing
    pub latest_pub_date: Option<String>,
}
