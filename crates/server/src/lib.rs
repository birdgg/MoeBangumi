pub mod api;
pub mod config;
pub mod db;
pub mod models;
pub mod openapi;
pub mod repositories;
pub mod services;
pub mod state;

use std::net::SocketAddr;
use std::path::Path;

use tower_http::services::{ServeDir, ServeFile};
use utoipa_scalar::{Scalar, Servable};

pub use api::create_router;
pub use config::{Config, Environment};
pub use db::create_pool;
pub use services::SettingsService;
pub use state::AppState;

const STATIC_DIR: &str = "/app/dist";

pub async fn run_server(
    addr: SocketAddr,
    env: Environment,
    data_path: &str,
    tmdb_api_key: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::new(env, data_path, tmdb_api_key.to_string());

    // Ensure data directories exist
    std::fs::create_dir_all(&config.data_path)?;
    std::fs::create_dir_all(config.posters_path())?;

    let pool = create_pool(&config.database_url).await?;
    let settings = SettingsService::new(&config).await?;
    let state = AppState::new(pool, config, settings);
    let (router, api) = create_router(state);

    let app = router.merge(Scalar::with_url("/docs", api));

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
