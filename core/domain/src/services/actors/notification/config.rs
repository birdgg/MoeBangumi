/// 通知配置
#[derive(Debug, Clone)]
pub struct NotificationConfig {
    pub enabled: bool,
    pub telegram: TelegramConfig,
}

/// Telegram 配置
#[derive(Debug, Clone)]
pub struct TelegramConfig {
    pub enabled: bool,
    pub bot_token: String,
    pub chat_id: String,
}
