//! BGM.tv metadata provider adapter

use std::sync::Arc;

use async_trait::async_trait;
use bgmtv::{BgmtvClient, ParsedSubject};

use crate::{MetadataProvider, MetadataSource, ProviderError, SearchQuery, SearchedMetadata};

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
        Ok(results.into_iter().map(SearchedMetadata::from).collect())
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
