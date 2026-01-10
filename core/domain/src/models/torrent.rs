use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

/// Torrent entity representing a BitTorrent file
/// All metadata (episode, subtitle_group, subtitle_languages, resolution)
/// is parsed from torrent_url on demand, not stored
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Torrent {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Optional reference to source RSS
    pub rss_id: Option<i64>,

    /// BitTorrent info hash (40-char hex for v1, 64-char for v2)
    pub info_hash: String,

    /// Torrent URL (.torrent file URL or magnet link)
    pub torrent_url: String,
}

/// Torrent with associated bangumi IDs
/// Used for queries that need to know which bangumi a torrent belongs to
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct TorrentWithBangumi {
    #[serde(flatten)]
    pub torrent: Torrent,
    /// Associated bangumi IDs (1 for WebRip, multiple for BDRip)
    pub bangumi_ids: Vec<i64>,
}

/// Request body for creating a new torrent
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct CreateTorrent {
    /// Optional reference to source RSS
    pub rss_id: Option<i64>,
    /// BitTorrent info hash
    pub info_hash: String,
    /// Torrent URL (.torrent file URL or magnet link)
    pub torrent_url: String,
    /// Associated bangumi IDs (1 for WebRip, multiple for BDRip)
    pub bangumi_ids: Vec<i64>,
}

/// Request body for updating a torrent
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateTorrent {
    /// Torrent URL (cannot be cleared, only updated)
    pub torrent_url: Option<String>,
}
