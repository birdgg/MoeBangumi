pub mod config;
pub mod db;
pub mod handlers;
pub mod models;
pub mod openapi;
pub mod router;
pub mod state;

use std::net::SocketAddr;

use utoipa_scalar::{Scalar, Servable};

pub use config::Config;
pub use db::create_pool;
pub use router::create_router;
pub use state::AppState;

pub async fn run_server(addr: SocketAddr, database_url: &str) -> Result<(), Box<dyn std::error::Error>> {
    let pool = create_pool(database_url).await?;
    let config = Config::new(database_url.to_string());
    let state = AppState::new(pool, config);
    let (router, api) = create_router(state);

    let app = router.merge(Scalar::with_url("/docs", api));

    tracing::info!("Starting server on {}", addr);
    tracing::info!("API documentation available at http://{}/docs", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
