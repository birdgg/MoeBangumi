//! BGM.tv metadata provider adapter

use std::sync::Arc;

use async_trait::async_trait;
use bgmtv::BgmtvClient;

use crate::{
    parse_subject, parse_subject_detail, Episode, EpisodeType, MetadataProvider, MetadataSource,
    ParsedSubject, ProviderError, SearchQuery, SearchedMetadata,
};

/// BGM.tv metadata provider
pub struct BgmtvProvider {
    client: Arc<BgmtvClient>,
}

impl BgmtvProvider {
    pub fn new(client: Arc<BgmtvClient>) -> Self {
        Self { client }
    }
}

#[async_trait]
impl MetadataProvider for BgmtvProvider {
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError> {
        let results = self.client.search_bangumi(&query.keyword).await?;
        Ok(results
            .iter()
            .map(|s| SearchedMetadata::from(parse_subject(s)))
            .collect())
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
        Ok(Some(SearchedMetadata::from(parse_subject_detail(&subject))))
    }

    async fn get_episodes(&self, external_id: &str) -> Result<Vec<Episode>, ProviderError> {
        let subject_id: i64 = external_id.parse().unwrap_or(0);
        if subject_id == 0 {
            return Ok(vec![]);
        }

        let response = self.client.get_episodes(subject_id).await?;
        Ok(response.data.into_iter().map(Episode::from).collect())
    }

    async fn get_episode_offset(&self, external_id: &str) -> Result<i32, ProviderError> {
        let episodes = self.get_episodes(external_id).await?;

        // Find the first main episode
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

impl From<ParsedSubject> for SearchedMetadata {
    fn from(subject: ParsedSubject) -> Self {
        Self {
            source: MetadataSource::Bgmtv,
            external_id: subject.bgmtv_id.to_string(),
            title_chinese: subject.title_chinese,
            title_original: subject.title_japanese,
            year: subject.year,
            season: Some(subject.season),
            platform: subject.platform.parse().ok(),
            total_episodes: subject.total_episodes as i32,
            poster_url: Some(subject.poster_url),
            air_date: subject.air_date,
        }
    }
}

impl From<bgmtv::Episode> for Episode {
    fn from(ep: bgmtv::Episode) -> Self {
        Self {
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
}
