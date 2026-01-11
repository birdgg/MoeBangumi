use axum::{extract::State, Json};
use reqwest::{Client, Proxy};
use serde::Deserialize;

use crate::error::AppResult;
use crate::models::{Settings, UpdateSettings};
use crate::state::AppState;

#[derive(Debug, Deserialize)]
pub struct TestProxyRequest {
    pub url: String,
    #[serde(default)]
    pub username: Option<String>,
    #[serde(default)]
    pub password: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct TestNotificationRequest {
    pub bot_token: String,
    pub chat_id: String,
}

pub async fn get_settings(State(state): State<AppState>) -> Json<Settings> {
    Json(state.infra.settings.get())
}

pub async fn update_settings(
    State(state): State<AppState>,
    Json(payload): Json<UpdateSettings>,
) -> AppResult<Json<Settings>> {
    let settings = state.infra.settings.update(payload).await?;
    Ok(Json(settings))
}

pub async fn reset_settings(State(state): State<AppState>) -> AppResult<Json<Settings>> {
    let settings = state.infra.settings.reset().await?;
    Ok(Json(settings))
}

pub async fn test_proxy(Json(payload): Json<TestProxyRequest>) -> AppResult<&'static str> {
    // Build proxy with optional authentication
    let mut proxy = Proxy::all(&payload.url)
        .map_err(|e| crate::error::AppError::BadRequest(format!("Invalid proxy URL: {}", e)))?;

    if let Some(username) = &payload.username {
        if !username.is_empty() {
            let password = payload.password.as_deref().unwrap_or("");
            proxy = proxy.basic_auth(username, password);
        }
    }

    // Create a temporary client with the proxy
    let client = Client::builder()
        .proxy(proxy)
        .timeout(std::time::Duration::from_secs(10))
        .build()
        .map_err(|e| {
            crate::error::AppError::BadRequest(format!("Failed to build client: {}", e))
        })?;

    // Test by making a request to mikanani.me
    let response = client
        .get("https://mikanani.me/")
        .send()
        .await
        .map_err(|e| crate::error::AppError::Internal(format!("Proxy connection failed: {}", e)))?;

    if response.status().is_success() {
        tracing::info!("Proxy connection test successful");
        Ok("Proxy connection successful")
    } else {
        Err(crate::error::AppError::Internal(format!(
            "Proxy test failed with status: {}",
            response.status()
        )))
    }
}

pub async fn test_notification(
    State(state): State<AppState>,
    Json(payload): Json<TestNotificationRequest>,
) -> AppResult<&'static str> {
    // Validate inputs
    if payload.bot_token.is_empty() {
        return Err(crate::error::AppError::BadRequest(
            "Bot Token 不能为空".to_string(),
        ));
    }
    if payload.chat_id.is_empty() {
        return Err(crate::error::AppError::BadRequest(
            "Chat ID 不能为空".to_string(),
        ));
    }

    // Create a temporary Telegram notifier with the provided credentials
    let client = state.infra.http_client.get_client();
    let notifier = crate::notify::telegram::TelegramNotifier::new_with_client(
        client,
        &payload.bot_token,
        &payload.chat_id,
    )
    .map_err(|e| crate::error::AppError::BadRequest(format!("配置无效: {}", e)))?;

    // Send test message
    use crate::notify::Notifier;
    notifier
        .send_message("🔔 moe-bangumi 通知测试成功！")
        .await
        .map_err(|e| crate::error::AppError::Internal(format!("发送失败: {}", e)))?;

    tracing::info!("Telegram notification test successful");
    Ok("Notification sent successfully")
}
