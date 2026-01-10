use serde::Deserialize;

use crate::{
    models::{Movie, PaginatedResponse, SearchMultiResult, TvShow},
    TmdbClient,
};

#[derive(Debug, Deserialize)]
struct RawPaginatedResponse {
    page: i64,
    results: Vec<serde_json::Value>,
    total_pages: i64,
    total_results: i64,
}

impl TmdbClient {
    /// Search for movies and TV shows using the multi search endpoint.
    ///
    /// This method filters out person results and only returns movie and TV results.
    pub async fn search_multi(
        &self,
        query: &str,
    ) -> crate::Result<PaginatedResponse<SearchMultiResult>> {
        let url = self.url("/search/multi");
        let api_key = self.api_key();

        let response = self
            .client()
            .get(&url)
            .query(&[
                ("api_key", api_key.as_str()),
                ("language", self.lang.as_str()),
                ("query", query),
                ("include_adult", "false"),
            ])
            .send()
            .await?;

        let raw: RawPaginatedResponse = self.handle_response(response).await?;

        // Filter and convert results, keeping only tv and movie types
        let results: Vec<SearchMultiResult> = raw
            .results
            .into_iter()
            .filter_map(|value| {
                let media_type = value.get("media_type")?.as_str()?;
                match media_type {
                    "tv" => {
                        let tv: TvShow = serde_json::from_value(value).ok()?;
                        Some(SearchMultiResult::Tv(tv))
                    }
                    "movie" => {
                        let movie: Movie = serde_json::from_value(value).ok()?;
                        Some(SearchMultiResult::Movie(movie))
                    }
                    _ => None, // Filter out "person" and other types
                }
            })
            .collect();

        Ok(PaginatedResponse {
            page: raw.page,
            results,
            total_pages: raw.total_pages,
            total_results: raw.total_results,
        })
    }
}
