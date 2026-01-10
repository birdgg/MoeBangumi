//! BGM.tv metadata provider adapter (compatibility)

use std::str::FromStr;
use std::sync::Arc;

use async_trait::async_trait;
use bgmtv::BgmtvClient;
use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

use super::{MetadataProvider, ProviderError};
use crate::compat::MetadataSource;

/// Platform type for bangumi (TV, Movie, OVA)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    #[default]
    Tv,
    Movie,
    Ova,
}

impl Platform {
    pub fn as_str(&self) -> &'static str {
        match self {
            Platform::Tv => "tv",
            Platform::Movie => "movie",
            Platform::Ova => "ova",
        }
    }
}

impl FromStr for Platform {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "movie" | "劇場版" => Platform::Movie,
            "ova" => Platform::Ova,
            _ => Platform::Tv,
        })
    }
}

/// Search query parameters
#[derive(Debug, Clone)]
pub struct SearchQuery {
    pub keyword: String,
    pub year: Option<i32>,
}

impl SearchQuery {
    pub fn new(keyword: impl Into<String>) -> Self {
        Self {
            keyword: keyword.into(),
            year: None,
        }
    }

    pub fn with_year(mut self, year: i32) -> Self {
        self.year = Some(year);
        self
    }
}

/// Standardized metadata search result
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct SearchedMetadata {
    pub source: MetadataSource,
    pub external_id: String,
    pub title_chinese: Option<String>,
    pub title_original: Option<String>,
    pub year: Option<i32>,
    pub season: Option<i32>,
    pub platform: Option<Platform>,
    pub total_episodes: i32,
    pub poster_url: Option<String>,
    pub air_date: Option<String>,
}

impl SearchedMetadata {
    pub fn matches_year(&self, expected_year: i32) -> bool {
        self.year
            .map(|y| (y - expected_year).abs() <= 1)
            .unwrap_or(false)
    }
}

/// Episode type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "lowercase")]
pub enum EpisodeType {
    #[default]
    Main,
    Special,
    Opening,
    Ending,
}

/// Episode information from metadata provider
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Episode {
    pub id: i64,
    pub episode_type: EpisodeType,
    pub name: String,
    pub name_cn: String,
    pub sort: f64,
    pub ep: Option<f64>,
    pub air_date: String,
}

/// BGM.tv metadata provider
pub struct BgmtvProvider {
    client: Arc<BgmtvClient>,
}

impl BgmtvProvider {
    pub fn new(client: Arc<BgmtvClient>) -> Self {
        Self { client }
    }

    pub fn with_http_client(http_client: reqwest::Client) -> Self {
        Self {
            client: Arc::new(BgmtvClient::new(http_client)),
        }
    }
}

#[async_trait]
impl MetadataProvider for BgmtvProvider {
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError> {
        let results = self.client.search_bangumi(&query.keyword).await?;
        Ok(results.iter().map(|s| parse_subject(s)).collect())
    }

    async fn get_detail(
        &self,
        external_id: &str,
    ) -> Result<Option<SearchedMetadata>, ProviderError> {
        let subject_id: i64 = external_id.parse().unwrap_or(0);
        if subject_id == 0 {
            return Ok(None);
        }

        let subject = self.client.get_subject(subject_id).await?;
        Ok(Some(parse_subject_detail(&subject)))
    }

    async fn get_episodes(&self, external_id: &str) -> Result<Vec<Episode>, ProviderError> {
        let subject_id: i64 = external_id.parse().unwrap_or(0);
        if subject_id == 0 {
            return Ok(vec![]);
        }

        let response = self.client.get_episodes(subject_id).await?;
        Ok(response.data.into_iter().map(convert_episode).collect())
    }

    async fn get_episode_offset(&self, external_id: &str) -> Result<i32, ProviderError> {
        let episodes = self.get_episodes(external_id).await?;

        let first_main = episodes
            .iter()
            .find(|ep| ep.episode_type == EpisodeType::Main);

        let offset = match first_main {
            Some(ep) => {
                let ep_num = ep.ep.unwrap_or(ep.sort);
                (ep.sort - ep_num).floor() as i32
            }
            None => 0,
        };

        Ok(offset)
    }

    fn name(&self) -> &'static str {
        "bgmtv"
    }
}

fn convert_episode(ep: bgmtv::Episode) -> Episode {
    Episode {
        id: ep.id,
        episode_type: match ep.episode_type {
            bgmtv::EpisodeType::Main => EpisodeType::Main,
            bgmtv::EpisodeType::Special => EpisodeType::Special,
            bgmtv::EpisodeType::Opening => EpisodeType::Opening,
            bgmtv::EpisodeType::Ending => EpisodeType::Ending,
        },
        name: ep.name,
        name_cn: ep.name_cn,
        sort: ep.sort,
        ep: ep.ep,
        air_date: ep.airdate,
    }
}

fn parse_subject(subject: &bgmtv::Subject) -> SearchedMetadata {
    let year = subject
        .date
        .split('-')
        .next()
        .and_then(|y| y.parse().ok());

    let season = extract_season_from_name(&subject.name);

    let title_chinese = if subject.name_cn.is_empty() {
        None
    } else {
        Some(subject.name_cn.clone())
    };

    SearchedMetadata {
        source: MetadataSource::Bgmtv,
        external_id: subject.id.to_string(),
        title_chinese,
        title_original: Some(subject.name.clone()),
        year,
        season: Some(season),
        platform: subject.platform.parse().ok(),
        total_episodes: subject.eps as i32,
        poster_url: Some(subject.images.large.clone()),
        air_date: Some(subject.date.clone()),
    }
}

fn parse_subject_detail(subject: &bgmtv::SubjectDetail) -> SearchedMetadata {
    let year = subject
        .date
        .as_ref()
        .and_then(|d| d.split('-').next())
        .and_then(|y| y.parse().ok());

    let season = extract_season_from_name(&subject.name);

    let title_chinese = if subject.name_cn.is_empty() {
        None
    } else {
        Some(subject.name_cn.clone())
    };

    SearchedMetadata {
        source: MetadataSource::Bgmtv,
        external_id: subject.id.to_string(),
        title_chinese,
        title_original: Some(subject.name.clone()),
        year,
        season: Some(season),
        platform: subject.platform.as_ref().and_then(|p| p.parse().ok()),
        total_episodes: subject.total_episodes as i32,
        poster_url: Some(subject.images.large.clone()),
        air_date: subject.date.clone(),
    }
}

fn extract_season_from_name(name: &str) -> i32 {
    use regex::Regex;
    use std::sync::LazyLock;

    static SEASON_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"(?i)(?:第([2-9]|1[0-9]|20)(?:季|期))|(?:SEASON\s*([2-9]|1[0-9]|20))|(?:S([2-9]|1[0-9]|20)(?:\s|$))").unwrap()
    });

    if let Some(caps) = SEASON_PATTERN.captures(name) {
        for i in 1..=3 {
            if let Some(m) = caps.get(i) {
                if let Ok(season) = m.as_str().parse::<i32>() {
                    return season;
                }
            }
        }
    }

    1
}
