use crate::client::{BgmtvClient, USER_AGENT};
use crate::models::SubjectDetail;
use crate::parser::{parse_subject_detail, ParsedSubject};

impl BgmtvClient {
    /// Get subject details by ID (returns parsed subject)
    /// GET /v0/subjects/{subject_id}
    pub async fn get_subject(&self, subject_id: i64) -> crate::Result<ParsedSubject> {
        let url = self.url(&format!("/v0/subjects/{}", subject_id));
        let client = self.client().await?;
        let response = client
            .get(&url)
            .header("User-Agent", USER_AGENT)
            .send()
            .await?;
        let detail: SubjectDetail = self.handle_response(response).await?;
        Ok(parse_subject_detail(&detail))
    }
}
