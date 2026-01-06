use crate::client::{BgmtvClient, USER_AGENT};
use crate::models::EpisodesResponse;

impl BgmtvClient {
    /// Get episodes by subject ID
    /// GET /v0/episodes?subject_id={subject_id}
    pub async fn get_episodes(&self, subject_id: i64) -> crate::Result<EpisodesResponse> {
        let url = self.url(&format!("/v0/episodes?subject_id={}", subject_id));
        let response = self
            .client()
            .get(&url)
            .header("User-Agent", USER_AGENT)
            .send()
            .await?;
        self.handle_response(response).await
    }

    /// Get episodes by subject ID with pagination
    /// GET /v0/episodes?subject_id={subject_id}&limit={limit}&offset={offset}
    pub async fn get_episodes_with_pagination(
        &self,
        subject_id: i64,
        limit: Option<i64>,
        offset: Option<i64>,
    ) -> crate::Result<EpisodesResponse> {
        let mut url = format!("/v0/episodes?subject_id={}", subject_id);
        if let Some(limit) = limit {
            url.push_str(&format!("&limit={}", limit));
        }
        if let Some(offset) = offset {
            url.push_str(&format!("&offset={}", offset));
        }
        let response = self
            .client()
            .get(&self.url(&url))
            .header("User-Agent", USER_AGENT)
            .send()
            .await?;
        self.handle_response(response).await
    }
}
