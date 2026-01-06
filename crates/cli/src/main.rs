use server::{create_log_channel, default_data_path, print_banner, DatabaseLayer, Environment};
use std::env;
use std::net::SocketAddr;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
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

    print_banner(env!("APP_VERSION"));

    let app_env = Environment::from_str(&env::var("APP_ENV").unwrap_or_default());
    let port: u16 = env::var("PORT")
        .unwrap_or_else(|_| "3000".to_string())
        .parse()?;
    let data_path = default_data_path();

    let addr: SocketAddr = format!("0.0.0.0:{}", port).parse()?;

    let current_version = env!("APP_VERSION");
    server::run_server(addr, app_env, &data_path.to_string_lossy(), current_version, Some(log_receiver)).await
}
