use crate::{
    models::{SeasonDetail, TvSeriesDetail},
    TmdbClient,
};

impl TmdbClient {
    /// Get TV series details
    ///
    /// GET /tv/{series_id}
    pub async fn get_tv_series(&self, series_id: i64) -> crate::Result<TvSeriesDetail> {
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

    /// Get season details with episodes
    ///
    /// GET /tv/{series_id}/season/{season_number}
    pub async fn get_season(
        &self,
        series_id: i64,
        season_number: i32,
    ) -> crate::Result<SeasonDetail> {
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
