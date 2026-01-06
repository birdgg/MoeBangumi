use crate::{
    models::{PaginatedResponse, TvShow},
    TmdbClient,
};

#[derive(Debug, Default)]
pub struct DiscoverBangumiParams {
    pub with_text_query: Option<String>,
}

impl TmdbClient {
    pub async fn discover_bangumi(
        &self,
        params: DiscoverBangumiParams,
    ) -> crate::Result<PaginatedResponse<TvShow>> {
        let url = self.url("/discover/tv");
        let client = self.client().await?;

        let api_key = self.api_key();
        let mut request = client.get(&url).query(&[
            ("api_key", api_key.as_str()),
            ("language", self.lang.as_str()),
            ("with_genres", "16"),
        ]);

        if let Some(query) = &params.with_text_query {
            request = request.query(&[("with_text_query", query.as_str())]);
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }
}
