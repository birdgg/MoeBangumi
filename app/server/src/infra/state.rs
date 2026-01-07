//! Application state management.
//!
//! This module organizes the application's dependencies into logical groups
//! and provides the main `AppState` struct for dependency injection.

mod builders;
mod types;

use std::sync::Arc;

use sqlx::SqlitePool;

use crate::config::Config;
use domain::services::SettingsService;

pub use types::{AppActors, AppClients, AppInfra, AppServices};

use builders::{build_api_clients, build_http_client, build_services, create_client_provider};

/// Application state - organized into logical groups
#[derive(Clone)]
pub struct AppState {
    pub infra: Arc<AppInfra>,
    pub clients: Arc<AppClients>,
    pub services: AppServices,
    pub actors: AppActors,
}

impl AppState {
    /// Create a new AppState instance
    pub fn new(
        db: SqlitePool,
        config: Config,
        settings: SettingsService,
        current_version: &str,
    ) -> Self {
        let settings = Arc::new(settings);
        let config = Arc::new(config);

        // Build infrastructure layer
        let http_client = build_http_client(&settings);
        let client_provider = create_client_provider(Arc::clone(&http_client));

        // Build API clients
        let api_clients = build_api_clients(&settings, &http_client, &client_provider);

        // Build services
        let (services, metadata_actor, rss_processing) = build_services(
            &db,
            &config,
            &settings,
            &http_client,
            &client_provider,
            &api_clients,
            current_version,
        );

        // Build actors
        let actors = builders::build_actors(&db, &settings, &services, metadata_actor, rss_processing);

        // Build final state structures
        let infra = Arc::new(AppInfra {
            db,
            config,
            http_client,
            settings,
        });

        let clients = Arc::new(AppClients {
            mikan: api_clients.mikan,
            rss: api_clients.rss,
        });

        Self {
            infra,
            clients,
            services,
            actors,
        }
    }
}
