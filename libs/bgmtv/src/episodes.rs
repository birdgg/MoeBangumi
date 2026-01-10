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
}
