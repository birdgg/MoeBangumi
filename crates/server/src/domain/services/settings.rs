use std::path::PathBuf;
use thiserror::Error;
use tokio::sync::watch;

use crate::config::Config;
use crate::models::{Settings, UpdateSettings};

#[derive(Debug, Error)]
pub enum SettingsError {
    #[error("{operation} '{path}': {source}")]
    Io {
        operation: &'static str,
        path: String,
        source: std::io::Error,
    },
    #[error("Failed to parse TOML: {0}")]
    Parse(#[from] toml::de::Error),
    #[error("Failed to serialize TOML: {0}")]
    Serialize(#[from] toml::ser::Error),
}

/// Receiver for settings changes
pub type SettingsWatcher = watch::Receiver<Settings>;

pub struct SettingsService {
    settings_path: PathBuf,
    sender: watch::Sender<Settings>,
    receiver: watch::Receiver<Settings>,
}

impl SettingsService {
    /// Initialize the settings service.
    /// Creates default settings file if it doesn't exist.
    pub async fn new(config: &Config) -> Result<Self, SettingsError> {
        let settings_path = config.settings_path();
        let settings = Self::load_or_create(&settings_path).await?;
        let (sender, receiver) = watch::channel(settings);

        Ok(Self {
            settings_path,
            sender,
            receiver,
        })
    }

    /// Load settings from file, or create with defaults if file doesn't exist.
    /// Also writes back to file to ensure new fields (with default values) are persisted.
    async fn load_or_create(path: &PathBuf) -> Result<Settings, SettingsError> {
        match tokio::fs::read_to_string(path).await {
            Ok(content) => {
                let settings: Settings = toml::from_str(&content)?;
                // Write back to file to persist any new fields with default values
                Self::write_atomically(path, &settings).await?;
                Ok(settings)
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                // Ensure parent directory exists
                if let Some(parent) = path.parent() {
                    tokio::fs::create_dir_all(parent).await.map_err(|e| {
                        SettingsError::Io {
                            operation: "Failed to create settings directory",
                            path: parent.display().to_string(),
                            source: e,
                        }
                    })?;
                }

                let default = Settings::default();
                Self::write_atomically(path, &default).await?;
                Ok(default)
            }
            Err(e) => Err(SettingsError::Io {
                operation: "Failed to read settings file",
                path: path.display().to_string(),
                source: e,
            }),
        }
    }

    /// Write settings to file atomically using temp file + rename pattern.
    async fn write_atomically(path: &PathBuf, settings: &Settings) -> Result<(), SettingsError> {
        let toml_str = toml::to_string_pretty(settings)?;
        let tmp_path = path.with_extension("toml.tmp");
        tokio::fs::write(&tmp_path, &toml_str).await.map_err(|e| {
            SettingsError::Io {
                operation: "Failed to write settings temp file",
                path: tmp_path.display().to_string(),
                source: e,
            }
        })?;
        tokio::fs::rename(&tmp_path, path).await.map_err(|e| {
            SettingsError::Io {
                operation: "Failed to rename settings file",
                path: path.display().to_string(),
                source: e,
            }
        })?;
        Ok(())
    }

    /// Get current settings (fast, no I/O).
    pub fn get(&self) -> Settings {
        self.receiver.borrow().clone()
    }

    /// Subscribe to settings changes.
    /// Returns a receiver that will be notified when settings change.
    pub fn subscribe(&self) -> SettingsWatcher {
        self.receiver.clone()
    }

    /// Update settings with partial data.
    /// Saves to file and broadcasts to all subscribers.
    pub async fn update(&self, data: UpdateSettings) -> Result<Settings, SettingsError> {
        let current = self.receiver.borrow().clone();
        let new_settings = current.merge(data);

        // Save to file first
        self.save_to_file(&new_settings).await?;

        // Broadcast to all subscribers (ignore error if no receivers)
        let _ = self.sender.send(new_settings.clone());

        Ok(new_settings)
    }

    /// Reset settings to defaults.
    pub async fn reset(&self) -> Result<Settings, SettingsError> {
        let default = Settings::default();

        // Save to file first
        self.save_to_file(&default).await?;

        // Broadcast to all subscribers
        let _ = self.sender.send(default.clone());

        Ok(default)
    }

    /// Save settings to TOML file atomically.
    async fn save_to_file(&self, settings: &Settings) -> Result<(), SettingsError> {
        Self::write_atomically(&self.settings_path, settings).await?;
        tracing::debug!("Saved settings to {}", self.settings_path.display());
        Ok(())
    }
}
