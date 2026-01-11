use crate::{
    models::{MediaType, PaginatedResponse, SearchResultItem},
    TmdbClient,
};

/// Search multi parameters
#[derive(Debug, Default)]
pub struct SearchMultiParams {
    pub query: String,
    pub page: Option<i64>,
}

impl TmdbClient {
    /// Search multi (unified search for movies and TV shows)
    ///
    /// GET /search/multi
    ///
    /// Note: Results are automatically filtered to only include TV and Movie types.
    pub async fn search_multi(
        &self,
        params: SearchMultiParams,
    ) -> crate::Result<PaginatedResponse<SearchResultItem>> {
        let url = self.url("/search/multi");
        let api_key = self.api_key();

        let page_str = params.page.map(|p| p.to_string());

        let mut query_params = vec![
            ("api_key", api_key.as_str()),
            ("language", self.lang.as_str()),
            ("query", params.query.as_str()),
        ];

        if let Some(ref page) = page_str {
            query_params.push(("page", page.as_str()));
        }

        let response = self.client().get(&url).query(&query_params).send().await?;

        let mut search_response: PaginatedResponse<SearchResultItem> =
            self.handle_response(response).await?;

        // Filter to only include TV and Movie results
        search_response
            .results
            .retain(|item| matches!(item.media_type, MediaType::Tv | MediaType::Movie));

        Ok(search_response)
    }
}
