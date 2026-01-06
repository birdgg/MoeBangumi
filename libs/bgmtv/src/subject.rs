use crate::client::{BgmtvClient, USER_AGENT};
use crate::models::SubjectDetail;

impl BgmtvClient {
    /// Get subject details by ID (returns raw subject detail)
    /// GET /v0/subjects/{subject_id}
    pub async fn get_subject(&self, subject_id: i64) -> crate::Result<SubjectDetail> {
        let url = self.url(&format!("/v0/subjects/{}", subject_id));
        let response = self
            .client()
            .get(&url)
            .header("User-Agent", USER_AGENT)
            .send()
            .await?;
        self.handle_response(response).await
    }
}
