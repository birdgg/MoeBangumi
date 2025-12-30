use crate::client::{BgmtvClient, USER_AGENT};
use crate::models::CalendarDay;

impl BgmtvClient {
    /// Get calendar (weekly anime schedule)
    /// GET /calendar
    pub async fn get_calendar(&self) -> crate::Result<Vec<CalendarDay>> {
        let url = self.url("/calendar");
        let client = self.client().await?;
        let response = client
            .get(&url)
            .header("User-Agent", USER_AGENT)
            .send()
            .await?;
        self.handle_response(response).await
    }
}
