use crate::client::QBittorrentClient;
use crate::error::QBittorrentError;

impl QBittorrentClient {
    /// Login to qBittorrent WebUI
    /// POST /api/v2/auth/login
    pub async fn login(&self, username: &str, password: &str) -> crate::Result<()> {
        let url = self.url("/auth/login");
        let params = [("username", username), ("password", password)];

        let response = self.client().post(&url).form(&params).send().await?;

        let status = response.status();

        // Extract SID from Set-Cookie header
        if let Some(cookie) = response.headers().get(reqwest::header::SET_COOKIE) {
            if let Ok(cookie_str) = cookie.to_str() {
                // Parse "SID=xxx; path=/" format
                if let Some(sid) = cookie_str
                    .split(';')
                    .next()
                    .and_then(|s| s.strip_prefix("SID="))
                {
                    self.set_sid(sid.to_string()).await;
                    tracing::debug!("Saved SID from login response");
                }
            }
        }

        let body = response.text().await.unwrap_or_default();

        if status.is_success() && body == "Ok." {
            tracing::debug!("Successfully logged in to qBittorrent");
            Ok(())
        } else if body == "Fails." {
            Err(QBittorrentError::Auth("Invalid username or password".into()))
        } else {
            Err(QBittorrentError::Auth(format!(
                "Login failed: {} - {}",
                status.as_u16(),
                body
            )))
        }
    }

    /// Logout from qBittorrent WebUI
    /// POST /api/v2/auth/logout
    pub async fn logout(&self) -> crate::Result<()> {
        let url = self.url("/auth/logout");

        let mut request = self.client().post(&url);

        // Add SID cookie if authenticated
        if let Some(sid) = self.get_sid().await {
            request = request.header(reqwest::header::COOKIE, format!("SID={}", sid));
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }
}
