use crate::client::{BgmtvClient, USER_AGENT};
use crate::models::{SearchFilter, SearchSubjectsRequest, SearchSubjectsResponse, SubjectType};
use crate::parser::{parse_subject, ParsedSubject};

impl BgmtvClient {
    /// Search subjects (returns raw response)
    /// POST /v0/search/subjects
    pub async fn search_subjects(
        &self,
        request: SearchSubjectsRequest,
    ) -> crate::Result<SearchSubjectsResponse> {
        let url = self.url("/v0/search/subjects");
        let response = self
            .client()
            .post(&url)
            .header("User-Agent", USER_AGENT)
            .json(&request)
            .send()
            .await?;
        self.handle_response(response).await
    }

    /// Search for Japanese anime (bangumi) - returns parsed subjects
    /// Convenience method with preset filter: type=[Anime], meta_tags=["日本"], air_date=[<today]
    pub async fn search_bangumi(
        &self,
        keyword: impl Into<String>,
    ) -> crate::Result<Vec<ParsedSubject>> {
        // Get today's date in YYYY-MM-DD format
        let today = chrono::Utc::now().format("%Y-%m-%d").to_string();

        let request = SearchSubjectsRequest {
            keyword: keyword.into(),
            filter: Some(SearchFilter {
                subject_type: Some(vec![SubjectType::Anime]),
                meta_tags: Some(vec!["日本".to_string()]),
                air_date: Some(vec![format!("<{}", today)]),
            }),
        };
        let response = self.search_subjects(request).await?;
        Ok(response.data.into_iter().map(|s| parse_subject(&s)).collect())
    }
}
