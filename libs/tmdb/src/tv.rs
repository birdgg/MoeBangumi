use crate::{
    models::{SeasonDetails, TvShowDetails},
    TmdbClient,
};

impl TmdbClient {
    /// Get the details of a TV show by its ID.
    pub async fn get_tv_details(&self, series_id: i64) -> crate::Result<TvShowDetails> {
        let url = self.url(&format!("/tv/{}", series_id));
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

    /// Get the details of a TV season by series ID and season number.
    pub async fn get_season_details(
        &self,
        series_id: i64,
        season_number: i64,
    ) -> crate::Result<SeasonDetails> {
        let url = self.url(&format!("/tv/{}/season/{}", series_id, season_number));
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
