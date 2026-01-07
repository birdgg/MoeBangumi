use downloader::DownloaderConfig;
use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;
use washing::SubtitleLanguageSet;

use super::{Clearable, DownloaderType};

/// Application settings stored in TOML file
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct Settings {
    /// Downloader configuration
    #[serde(default)]
    pub downloader: DownloaderSettings,
    /// Filter configuration
    #[serde(default)]
    pub filter: FilterSettings,
    /// Proxy configuration for HTTP client
    #[serde(default)]
    pub proxy: ProxySettings,
    /// Notification configuration
    #[serde(default)]
    pub notification: NotificationSettings,
    /// Priority configuration for torrent selection and washing
    #[serde(default)]
    pub priority: PrioritySettings,
    /// TMDB API configuration
    #[serde(default)]
    pub tmdb: TmdbSettings,
}

/// Downloader configuration with per-type configs
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct DownloaderSettings {
    /// Currently active downloader type
    #[serde(default = "DownloaderSettings::default_type", rename = "type")]
    pub downloader_type: DownloaderType,
    /// Default save path for downloads (shared across all downloaders)
    #[serde(default = "DownloaderSettings::default_save_path")]
    pub save_path: String,
    /// Per-downloader configurations
    #[serde(default)]
    pub configs: DownloaderConfigs,
}

/// Per-downloader configurations
#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct DownloaderConfigs {
    /// qBittorrent configuration
    #[serde(default)]
    pub qbittorrent: QBittorrentConfig,
    /// Transmission configuration
    #[serde(default)]
    pub transmission: TransmissionConfig,
}

/// qBittorrent-specific configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct QBittorrentConfig {
    /// Web UI URL (e.g., http://localhost:8080)
    #[serde(default = "QBittorrentConfig::default_url")]
    pub url: String,
    /// Username (required)
    #[serde(default)]
    pub username: String,
    /// Password (required)
    #[serde(default)]
    pub password: String,
}

impl Default for QBittorrentConfig {
    fn default() -> Self {
        Self {
            url: Self::default_url(),
            username: String::new(),
            password: String::new(),
        }
    }
}

impl QBittorrentConfig {
    fn default_url() -> String {
        "http://localhost:8080".to_string()
    }
}

/// Transmission-specific configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct TransmissionConfig {
    /// RPC URL (e.g., http://localhost:9091/transmission/rpc)
    #[serde(default = "TransmissionConfig::default_url")]
    pub url: String,
    /// Username (optional)
    #[serde(default)]
    pub username: String,
    /// Password (optional)
    #[serde(default)]
    pub password: String,
}

impl Default for TransmissionConfig {
    fn default() -> Self {
        Self {
            url: Self::default_url(),
            username: String::new(),
            password: String::new(),
        }
    }
}

impl TransmissionConfig {
    fn default_url() -> String {
        "http://localhost:9091/transmission/rpc".to_string()
    }
}

impl Default for DownloaderSettings {
    fn default() -> Self {
        Self {
            downloader_type: Self::default_type(),
            save_path: Self::default_save_path(),
            configs: DownloaderConfigs::default(),
        }
    }
}

impl DownloaderSettings {
    fn default_type() -> DownloaderType {
        DownloaderType::QBittorrent
    }

    fn default_save_path() -> String {
        "/Media/Bangumi".to_string()
    }

    /// Get the active downloader's configuration
    pub fn get_active_config(&self) -> DownloaderConfig {
        match self.downloader_type {
            DownloaderType::QBittorrent => {
                let cfg = &self.configs.qbittorrent;
                DownloaderConfig {
                    downloader_type: DownloaderType::QBittorrent,
                    url: cfg.url.clone(),
                    username: Some(cfg.username.clone()),
                    password: Some(cfg.password.clone()),
                }
            }
            DownloaderType::Transmission => {
                let cfg = &self.configs.transmission;
                DownloaderConfig {
                    downloader_type: DownloaderType::Transmission,
                    url: cfg.url.clone(),
                    username: if cfg.username.is_empty() {
                        None
                    } else {
                        Some(cfg.username.clone())
                    },
                    password: if cfg.password.is_empty() {
                        None
                    } else {
                        Some(cfg.password.clone())
                    },
                }
            }
        }
    }

    /// Check if the active downloader config is complete
    pub fn is_active_config_complete(&self) -> bool {
        match self.downloader_type {
            DownloaderType::QBittorrent => {
                let cfg = &self.configs.qbittorrent;
                !cfg.url.is_empty() && !cfg.username.is_empty() && !cfg.password.is_empty()
            }
            DownloaderType::Transmission => {
                // Transmission only requires URL
                !self.configs.transmission.url.is_empty()
            }
        }
    }
}

/// Filter configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct FilterSettings {
    /// Global RSS filters (regex patterns to exclude)
    #[serde(default = "FilterSettings::default_global_rss_filters")]
    pub global_rss_filters: Vec<String>,
}

impl Default for FilterSettings {
    fn default() -> Self {
        Self {
            global_rss_filters: Self::default_global_rss_filters(),
        }
    }
}

impl FilterSettings {
    fn default_global_rss_filters() -> Vec<String> {
        vec![
            "720[Pp]".to_string(),
            "\\d-\\d".to_string(),
            "合集".to_string(),
        ]
    }
}

/// Priority configuration for torrent selection and washing
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct PrioritySettings {
    /// Subtitle groups in priority order (first = highest priority)
    #[serde(default = "PrioritySettings::default_subtitle_groups")]
    pub subtitle_groups: Vec<String>,
    /// Subtitle language combinations in priority order (first = highest priority)
    /// Each entry is a set of languages that must exactly match
    /// Example: [[CHS, JPN], [CHS, CHT, JPN], [CHT]]
    #[serde(default = "PrioritySettings::default_subtitle_language_sets")]
    pub subtitle_language_sets: Vec<SubtitleLanguageSet>,
}

impl Default for PrioritySettings {
    fn default() -> Self {
        Self {
            subtitle_groups: Self::default_subtitle_groups(),
            subtitle_language_sets: Self::default_subtitle_language_sets(),
        }
    }
}

impl PrioritySettings {
    fn default_subtitle_groups() -> Vec<String> {
        vec![]
    }

    fn default_subtitle_language_sets() -> Vec<SubtitleLanguageSet> {
        use parser::SubType;
        vec![
            SubtitleLanguageSet::new(vec![SubType::Chs]),                           // 简体
            SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Jpn]),              // 简日
            SubtitleLanguageSet::new(vec![SubType::Cht]),                           // 繁体
            SubtitleLanguageSet::new(vec![SubType::Cht, SubType::Jpn]),              // 繁日
            SubtitleLanguageSet::new(vec![SubType::Chs, SubType::Cht, SubType::Jpn]), // 简繁日
        ]
    }

    /// Convert to PriorityConfig for the priority calculator
    pub fn to_config(&self) -> washing::PriorityConfig {
        washing::PriorityConfig {
            subtitle_groups: self.subtitle_groups.clone(),
            subtitle_language_sets: self.subtitle_language_sets.clone(),
        }
    }
}

/// Proxy configuration for HTTP client
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct ProxySettings {
    /// Proxy server URL (e.g., http://127.0.0.1:7890 or socks5://127.0.0.1:1080)
    #[serde(default)]
    pub url: String,
    /// Proxy username (optional)
    #[serde(default)]
    pub username: String,
    /// Proxy password (optional)
    #[serde(default)]
    pub password: String,
}

impl Default for ProxySettings {
    fn default() -> Self {
        Self {
            url: String::new(),
            username: String::new(),
            password: String::new(),
        }
    }
}

/// TMDB API configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct TmdbSettings {
    /// TMDB API key
    #[serde(default)]
    pub api_key: String,
}

impl TmdbSettings {
    /// Check if TMDB API key is configured
    pub fn is_configured(&self) -> bool {
        !self.api_key.is_empty()
    }
}

/// Notification configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct NotificationSettings {
    /// Global enable/disable for notifications
    #[serde(default)]
    pub enabled: bool,
    /// Telegram configuration
    #[serde(default)]
    pub telegram: TelegramConfig,
}

impl Default for NotificationSettings {
    fn default() -> Self {
        Self {
            enabled: false,
            telegram: TelegramConfig::default(),
        }
    }
}

/// Telegram notification configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct TelegramConfig {
    /// Enable Telegram notifications
    #[serde(default)]
    pub enabled: bool,
    /// Telegram Bot API token
    #[serde(default)]
    pub bot_token: String,
    /// Telegram chat ID to send notifications to
    #[serde(default)]
    pub chat_id: String,
}

impl Settings {
    /// Merge update data into current settings
    pub fn merge(&self, update: UpdateSettings) -> Self {
        Self {
            downloader: if let Some(d) = update.downloader {
                DownloaderSettings {
                    downloader_type: d
                        .downloader_type
                        .unwrap_or(self.downloader.downloader_type),
                    save_path: d
                        .save_path
                        .resolve_or_empty(self.downloader.save_path.clone()),
                    configs: DownloaderConfigs {
                        qbittorrent: if let Some(qb) = d.qbittorrent {
                            QBittorrentConfig {
                                url: qb
                                    .url
                                    .resolve_or_empty(self.downloader.configs.qbittorrent.url.clone()),
                                username: qb.username.resolve_or_empty(
                                    self.downloader.configs.qbittorrent.username.clone(),
                                ),
                                password: qb.password.resolve_or_empty(
                                    self.downloader.configs.qbittorrent.password.clone(),
                                ),
                            }
                        } else {
                            self.downloader.configs.qbittorrent.clone()
                        },
                        transmission: if let Some(tr) = d.transmission {
                            TransmissionConfig {
                                url: tr.url.resolve_or_empty(
                                    self.downloader.configs.transmission.url.clone(),
                                ),
                                username: tr.username.resolve_or_empty(
                                    self.downloader.configs.transmission.username.clone(),
                                ),
                                password: tr.password.resolve_or_empty(
                                    self.downloader.configs.transmission.password.clone(),
                                ),
                            }
                        } else {
                            self.downloader.configs.transmission.clone()
                        },
                    },
                }
            } else {
                self.downloader.clone()
            },
            filter: FilterSettings {
                global_rss_filters: update
                    .filter
                    .and_then(|f| f.global_rss_filters)
                    .unwrap_or_else(|| self.filter.global_rss_filters.clone()),
            },
            proxy: ProxySettings {
                url: update
                    .proxy
                    .as_ref()
                    .map(|p| p.url.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.proxy.url.clone()),
                username: update
                    .proxy
                    .as_ref()
                    .map(|p| p.username.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.proxy.username.clone()),
                password: update
                    .proxy
                    .as_ref()
                    .map(|p| p.password.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.proxy.password.clone()),
            },
            notification: if let Some(n) = update.notification {
                NotificationSettings {
                    enabled: n.enabled.unwrap_or(self.notification.enabled),
                    telegram: if let Some(t) = n.telegram {
                        TelegramConfig {
                            enabled: t.enabled.unwrap_or(self.notification.telegram.enabled),
                            bot_token: t
                                .bot_token
                                .resolve_or_empty(self.notification.telegram.bot_token.clone()),
                            chat_id: t
                                .chat_id
                                .resolve_or_empty(self.notification.telegram.chat_id.clone()),
                        }
                    } else {
                        self.notification.telegram.clone()
                    },
                }
            } else {
                self.notification.clone()
            },
            priority: if let Some(p) = update.priority {
                PrioritySettings {
                    subtitle_groups: p
                        .subtitle_groups
                        .unwrap_or_else(|| self.priority.subtitle_groups.clone()),
                    subtitle_language_sets: p
                        .subtitle_language_sets
                        .unwrap_or_else(|| self.priority.subtitle_language_sets.clone()),
                }
            } else {
                self.priority.clone()
            },
            tmdb: if let Some(t) = update.tmdb {
                TmdbSettings {
                    api_key: t.api_key.resolve_or_empty(self.tmdb.api_key.clone()),
                }
            } else {
                self.tmdb.clone()
            },
        }
    }
}

/// Request body for updating settings.
/// All fields are optional - only provided fields will be updated.
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateSettings {
    /// Downloader configuration updates
    #[serde(default)]
    pub downloader: Option<UpdateDownloaderSettings>,
    /// Filter configuration updates
    #[serde(default)]
    pub filter: Option<UpdateFilterSettings>,
    /// Proxy configuration updates
    #[serde(default)]
    pub proxy: Option<UpdateProxySettings>,
    /// Notification configuration updates
    #[serde(default)]
    pub notification: Option<UpdateNotificationSettings>,
    /// Priority configuration updates
    #[serde(default)]
    pub priority: Option<UpdatePrioritySettings>,
    /// TMDB configuration updates
    #[serde(default)]
    pub tmdb: Option<UpdateTmdbSettings>,
}

/// Request body for updating downloader settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateDownloaderSettings {
    /// Switch active downloader type
    #[serde(default, rename = "type")]
    pub downloader_type: Option<DownloaderType>,
    /// Update shared save path (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub save_path: Clearable<String>,
    /// Update qBittorrent config
    #[serde(default)]
    pub qbittorrent: Option<UpdateQBittorrentConfig>,
    /// Update Transmission config
    #[serde(default)]
    pub transmission: Option<UpdateTransmissionConfig>,
}

/// Request body for updating qBittorrent settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateQBittorrentConfig {
    /// Web UI URL (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub url: Clearable<String>,
    /// Username (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub username: Clearable<String>,
    /// Password (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub password: Clearable<String>,
}

/// Request body for updating Transmission settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateTransmissionConfig {
    /// RPC URL (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub url: Clearable<String>,
    /// Username (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub username: Clearable<String>,
    /// Password (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub password: Clearable<String>,
}

/// Request body for updating filter settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateFilterSettings {
    /// Global RSS filters (replaces entire array if provided)
    #[serde(default)]
    pub global_rss_filters: Option<Vec<String>>,
}

/// Request body for updating proxy settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateProxySettings {
    /// Proxy server URL (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub url: Clearable<String>,
    /// Username (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub username: Clearable<String>,
    /// Password (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub password: Clearable<String>,
}

/// Request body for updating notification settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateNotificationSettings {
    /// Enable/disable notifications globally
    #[serde(default)]
    pub enabled: Option<bool>,
    /// Telegram configuration updates
    #[serde(default)]
    pub telegram: Option<UpdateTelegramConfig>,
}

/// Request body for updating Telegram settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateTelegramConfig {
    /// Enable Telegram notifications
    #[serde(default)]
    pub enabled: Option<bool>,
    /// Bot token (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub bot_token: Clearable<String>,
    /// Chat ID (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub chat_id: Clearable<String>,
}

/// Request body for updating priority settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdatePrioritySettings {
    /// Subtitle groups in priority order (replaces entire array if provided)
    #[serde(default)]
    pub subtitle_groups: Option<Vec<String>>,
    /// Subtitle language combinations in priority order (replaces entire array if provided)
    /// Each entry is a set of languages that must exactly match
    #[serde(default)]
    pub subtitle_language_sets: Option<Vec<SubtitleLanguageSet>>,
}

/// Request body for updating TMDB settings
#[derive(Debug, Clone, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct UpdateTmdbSettings {
    /// TMDB API key (send null to clear)
    #[serde(default)]
    #[cfg_attr(feature = "openapi", schema(value_type = Option<String>))]
    pub api_key: Clearable<String>,
}
