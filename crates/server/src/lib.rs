pub mod api;
pub mod config;
pub mod db;
pub mod error;
pub mod models;
pub mod openapi;
pub mod repositories;
pub mod seed;
pub mod services;
pub mod state;

use std::net::SocketAddr;
use std::path::Path;

use tower_http::services::{ServeDir, ServeFile};
use utoipa_scalar::{Scalar, Servable};

pub use api::create_router;
pub use config::{Config, Environment};
pub use db::create_pool;
pub use error::{AppError, AppResult};
pub use services::{create_log_channel, start_log_writer, DatabaseLayer, LogReceiver, SettingsService};
pub use state::AppState;

const STATIC_DIR: &str = "/app/dist";

pub async fn run_server(
    addr: SocketAddr,
    env: Environment,
    data_path: &str,
    tmdb_api_key: &str,
    log_receiver: Option<LogReceiver>,
) -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::new(env, data_path, tmdb_api_key.to_string());

    // Ensure data directories exist
    std::fs::create_dir_all(&config.data_path)?;
    std::fs::create_dir_all(config.posters_path())?;

    let pool = create_pool(&config.database_url).await?;

    // Seed database in dev environment
    if env.is_dev() {
        if let Err(e) = seed::seed_bangumi(&pool).await {
            tracing::warn!("Failed to seed database: {}", e);
        }
    }

    let settings = SettingsService::new(&config).await?;
    let posters_path = config.posters_path();
    let state = AppState::new(pool, config, settings);

    // Start notification worker
    if let Err(e) = state.notification.start().await {
        tracing::warn!("Failed to start notification service: {}", e);
    }

    // Start log writer if receiver is provided
    if let Some(receiver) = log_receiver {
        start_log_writer(receiver, state.logs.clone());
    }

    let (router, api) = create_router(state);

    // Serve poster images from data directory
    let app = router
        .nest_service("/posters", ServeDir::new(&posters_path))
        .merge(Scalar::with_url("/docs", api));

    // Serve static files if the dist directory exists (in Docker)
    let app = if Path::new(STATIC_DIR).exists() {
        tracing::info!("Serving static files from {}", STATIC_DIR);
        let serve_dir = ServeDir::new(STATIC_DIR)
            .not_found_service(ServeFile::new(format!("{}/index.html", STATIC_DIR)));
        app.fallback_service(serve_dir)
    } else {
        app
    };

    tracing::info!("Starting server on {}", addr);
    tracing::info!("Environment: {:?}", env);
    tracing::info!("Data path: {}", data_path);
    tracing::info!("API documentation available at http://{}/docs", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app.into_make_service()).await?;

    Ok(())
}
