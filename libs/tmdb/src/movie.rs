use crate::{models::MovieDetails, TmdbClient};

impl TmdbClient {
    /// Get the details of a movie by its ID.
    pub async fn get_movie_details(&self, movie_id: i64) -> crate::Result<MovieDetails> {
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
