use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct BangumiDetail {
    pub subgroups: Vec<Subgroup>,
}

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Subgroup {
    pub id: String,
    pub name: String,
    pub rss_url: String,
    pub episodes: Vec<Episode>,
}

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Episode {
    pub name: String,
    pub torrent_url: Option<String>,
    // 解析后的元数据
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sub_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolution: Option<String>,
}
