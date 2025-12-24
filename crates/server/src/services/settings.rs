use std::path::PathBuf;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;

use crate::config::Config;
use crate::models::{Settings, UpdateSettings};

#[derive(Debug, Error)]
pub enum SettingsError {
    #[error("Failed to read settings file: {0}")]
    Io(#[from] std::io::Error),
    #[error("Failed to parse TOML: {0}")]
    Parse(#[from] toml::de::Error),
    #[error("Failed to serialize TOML: {0}")]
    Serialize(#[from] toml::ser::Error),
}

pub struct SettingsService {
    settings_path: PathBuf,
    cache: Arc<RwLock<Settings>>,
}

impl SettingsService {
    /// Initialize the settings service.
    /// Creates default settings file if it doesn't exist.
    pub async fn new(config: &Config) -> Result<Self, SettingsError> {
        let settings_path = config.settings_path();
        let settings = Self::load_or_create(&settings_path).await?;

        Ok(Self {
            settings_path,
            cache: Arc::new(RwLock::new(settings)),
        })
    }

    /// Load settings from file, or create with defaults if file doesn't exist.
    async fn load_or_create(path: &PathBuf) -> Result<Settings, SettingsError> {
        match tokio::fs::read_to_string(path).await {
            Ok(content) => {
                let settings: Settings = toml::from_str(&content)?;
                tracing::info!("Loaded settings from {}", path.display());
                Ok(settings)
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                // Ensure parent directory exists
                if let Some(parent) = path.parent() {
                    tokio::fs::create_dir_all(parent).await?;
                }

                let default = Settings::default();
                let toml_str = toml::to_string_pretty(&default)?;
                tokio::fs::write(path, toml_str).await?;
                tracing::info!("Created default settings file at {}", path.display());
                Ok(default)
            }
            Err(e) => Err(e.into()),
        }
    }

    /// Get current settings from cache (fast, no I/O).
    pub async fn get(&self) -> Settings {
        self.cache.read().await.clone()
    }

    /// Update settings with partial data.
    /// Saves to file and updates cache atomically.
    pub async fn update(&self, data: UpdateSettings) -> Result<Settings, SettingsError> {
        // Read current settings outside of write lock
        let current = self.cache.read().await.clone();

        // Merge updates with existing settings
        let new_settings = current.merge(data);

        // Save to file first (fail-safe: only update cache if file write succeeds)
        // This is done outside the write lock to avoid blocking reads during I/O
        self.save_to_file(&new_settings).await?;

        // Only acquire write lock after file is saved successfully
        *self.cache.write().await = new_settings.clone();

        Ok(new_settings)
    }

    /// Reset settings to defaults.
    pub async fn reset(&self) -> Result<Settings, SettingsError> {
        let default = Settings::default();

        // Save to file first
        self.save_to_file(&default).await?;

        // Update cache after file is saved
        *self.cache.write().await = default.clone();

        Ok(default)
    }

    /// Save settings to TOML file atomically.
    /// Uses write-to-temp-then-rename pattern for crash safety.
    async fn save_to_file(&self, settings: &Settings) -> Result<(), SettingsError> {
        let toml_str = toml::to_string_pretty(settings)?;

        // Write to temporary file first
        let tmp_path = self.settings_path.with_extension("toml.tmp");
        tokio::fs::write(&tmp_path, &toml_str).await?;

        // Atomically rename temp file to target
        tokio::fs::rename(&tmp_path, &self.settings_path).await?;

        tracing::debug!("Saved settings to {}", self.settings_path.display());
        Ok(())
    }
}
