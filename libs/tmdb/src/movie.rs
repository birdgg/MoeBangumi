use crate::{models::MovieDetail, TmdbClient};

impl TmdbClient {
    /// Get movie details
    ///
    /// GET /movie/{movie_id}
    pub async fn get_movie(&self, movie_id: i64) -> crate::Result<MovieDetail> {
        let url = self.url(&format!("/movie/{}", movie_id));
        let api_key = self.api_key();
        let response = self
            .client()
            .get(&url)
            .query(&[
                ("api_key", api_key.as_str()),
                ("language", self.lang.as_str()),
            ])
            .send()
            .await?;
        self.handle_response(response).await
    }
}
