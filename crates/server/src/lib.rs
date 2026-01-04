pub mod api;
pub mod banner;
pub mod config;
pub mod db;
pub mod error;
pub mod models;
#[cfg(feature = "openapi")]
pub mod openapi;
pub mod repositories;
pub mod services;
pub mod state;
pub mod utils;

use std::net::SocketAddr;
use std::path::Path;

use tower_http::services::{ServeDir, ServeFile};
#[cfg(feature = "openapi")]
use utoipa_scalar::{Scalar, Servable};

pub use api::create_router;
pub use banner::print_banner;
pub use config::{default_data_path, Config, Environment};
pub use db::{create_pool, DatabaseError};
pub use error::{AppError, AppResult};
pub use services::{
    create_log_channel, start_log_writer, DatabaseLayer, LogReceiver, SettingsService,
};
pub use state::AppState;
pub use utils::SeasonIterator;

const STATIC_DIR: &str = "/app/dist";

pub async fn run_server(
    addr: SocketAddr,
    env: Environment,
    data_path: &str,
    log_receiver: Option<LogReceiver>,
) -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::new(env, data_path);

    // Ensure data directories exist
    std::fs::create_dir_all(&config.data_path).map_err(|e| {
        format!(
            "Failed to create data directory '{}': {} (check directory permissions)",
            config.data_path.display(),
            e
        )
    })?;
    std::fs::create_dir_all(config.posters_path()).map_err(|e| {
        format!(
            "Failed to create posters directory '{}': {} (check directory permissions)",
            config.posters_path().display(),
            e
        )
    })?;

    let pool = create_pool(&config.database_url).await?;

    let settings = SettingsService::new(&config).await?;
    let posters_path = config.posters_path();
    let state = AppState::new(pool, config, settings);

    // Import calendar seed data if needed (first startup)
    if state.calendar.needs_seed_import().await.unwrap_or(false) {
        tracing::info!("Importing calendar seed data...");
        match state.calendar.import_seed_data().await {
            Ok(count) => tracing::info!("Imported {} calendar entries from seed data", count),
            Err(e) => tracing::warn!("Failed to import calendar seed data: {}", e),
        }
    }

    // Note: Notification service is now Actor-based and starts automatically

    // Start log writer if receiver is provided
    if let Some(receiver) = log_receiver {
        start_log_writer(receiver, state.logs.clone());
    }

    // Serve poster images from data directory
    #[cfg(feature = "openapi")]
    let app = {
        let (router, api) = create_router(state);
        router
            .nest_service("/posters", ServeDir::new(&posters_path))
            .merge(Scalar::with_url("/docs", api))
    };

    #[cfg(not(feature = "openapi"))]
    let app = {
        let router = create_router(state);
        router.nest_service("/posters", ServeDir::new(&posters_path))
    };

    // Serve static files if the dist directory exists (in Docker)
    let app = if Path::new(STATIC_DIR).exists() {
        tracing::info!("Serving static files from {}", STATIC_DIR);
        let serve_dir = ServeDir::new(STATIC_DIR)
            .not_found_service(ServeFile::new(format!("{}/index.html", STATIC_DIR)));
        app.fallback_service(serve_dir)
    } else {
        app
    };

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app.into_make_service()).await?;

    Ok(())
}
