use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{Settings, UpdateSettings};

/// Common SELECT fields for settings queries
const SELECT_SETTINGS: &str = r#"
    SELECT
        id, created_at, updated_at,
        qb_url, qb_username, qb_password,
        global_rss_filters
    FROM settings
"#;

pub struct SettingsRepository;

impl SettingsRepository {
    /// Get application settings (always returns the singleton row)
    pub async fn get(pool: &SqlitePool) -> Result<Settings, sqlx::Error> {
        let query = format!("{} WHERE id = 1", SELECT_SETTINGS);
        let row = sqlx::query_as::<_, SettingsRow>(&query)
            .fetch_one(pool)
            .await?;

        Ok(row.into())
    }

    /// Update application settings
    pub async fn update(pool: &SqlitePool, data: UpdateSettings) -> Result<Settings, sqlx::Error> {
        // Get current settings to merge with updates
        let existing = Self::get(pool).await?;

        let qb_url = data.qb_url.resolve(existing.qb_url);
        let qb_username = data.qb_username.resolve(existing.qb_username);
        let qb_password = data.qb_password.resolve(existing.qb_password);
        let global_rss_filters = data
            .global_rss_filters
            .unwrap_or(existing.global_rss_filters);

        // Serialize filters to JSON
        let filters_json =
            serde_json::to_string(&global_rss_filters).unwrap_or_else(|_| "[]".to_string());

        sqlx::query(
            r#"
            UPDATE settings SET
                qb_url = $1,
                qb_username = $2,
                qb_password = $3,
                global_rss_filters = $4
            WHERE id = 1
            "#,
        )
        .bind(&qb_url)
        .bind(&qb_username)
        .bind(&qb_password)
        .bind(&filters_json)
        .execute(pool)
        .await?;

        Self::get(pool).await
    }

    /// Reset settings to defaults (clear all configuration)
    pub async fn reset(pool: &SqlitePool) -> Result<Settings, sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE settings SET
                qb_url = NULL,
                qb_username = NULL,
                qb_password = NULL,
                global_rss_filters = '[]'
            WHERE id = 1
            "#,
        )
        .execute(pool)
        .await?;

        Self::get(pool).await
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct SettingsRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    qb_url: Option<String>,
    qb_username: Option<String>,
    qb_password: Option<String>,
    global_rss_filters: String,
}

impl From<SettingsRow> for Settings {
    fn from(row: SettingsRow) -> Self {
        let global_rss_filters =
            serde_json::from_str(&row.global_rss_filters).unwrap_or_else(|e| {
                tracing::warn!("Failed to parse global_rss_filters: {}", e);
                vec![]
            });

        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            qb_url: row.qb_url,
            qb_username: row.qb_username,
            qb_password: row.qb_password,
            global_rss_filters,
        }
    }
}
