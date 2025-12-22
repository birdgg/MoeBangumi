use std::env;
use std::net::SocketAddr;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    dotenvy::dotenv().ok();

    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    let port: u16 = env::var("PORT")
        .unwrap_or_else(|_| "3000".to_string())
        .parse()?;
    let database = env::var("DATABASE").unwrap_or_else(|_| "todos.db".to_string());

    let addr: SocketAddr = format!("127.0.0.1:{}", port).parse()?;
    let database_url = format!("sqlite:{}?mode=rwc", database);

    server::run_server(addr, &database_url).await
}
