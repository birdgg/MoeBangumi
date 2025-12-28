use server::{create_log_channel, DatabaseLayer, Environment};
use std::env;
use std::net::SocketAddr;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    dotenvy::dotenv().ok();

    // Create log channel for database layer
    let (log_sender, log_receiver) = create_log_channel();

    // Initialize tracing with database layer
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .with(tracing_subscriber::fmt::layer())
        .with(DatabaseLayer::new(log_sender))
        .init();

    let app_env = Environment::from_str(&env::var("APP_ENV").unwrap_or_default());
    let port: u16 = env::var("PORT")
        .unwrap_or_else(|_| "3000".to_string())
        .parse()?;
    let data_path = env::var("DATA_PATH")
        .unwrap_or_else(|_| app_env.default_data_path().to_string_lossy().to_string());
    let tmdb_api_key = env::var("TMDB_API_KEY").expect("TMDB_API_KEY must be set");

    let addr: SocketAddr = format!("0.0.0.0:{}", port).parse()?;

    server::run_server(addr, app_env, &data_path, &tmdb_api_key, Some(log_receiver)).await
}
