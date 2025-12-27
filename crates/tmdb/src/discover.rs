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
        let mut url = self.url("/discover/tv");
        url.push_str(&format!(
            "?api_key={}&language={}&with_genres=16",
            self.api_key, self.lang
        ));

        if let Some(query) = &params.with_text_query {
            url.push_str(&format!("&with_text_query={}", urlencoding::encode(query)));
        }
        let client = self.client().await?;
        let response = client.get(&url).send().await?;
        self.handle_response(response).await
    }
}
