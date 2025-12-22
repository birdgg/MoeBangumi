use clap::Parser;
use std::net::SocketAddr;

#[derive(Parser)]
#[command(name = "todo")]
#[command(about = "A simple todo server", long_about = None)]
struct Cli {
    /// Port to listen on
    #[arg(short, long, default_value = "3000")]
    port: u16,

    /// Host to bind to
    #[arg(long, default_value = "127.0.0.1")]
    host: String,

    /// Database file path
    #[arg(short, long, default_value = "todos.db")]
    database: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    let cli = Cli::parse();

    let addr: SocketAddr = format!("{}:{}", cli.host, cli.port).parse()?;
    let database_url = format!("sqlite:{}?mode=rwc", cli.database);

    server::run_server(addr, &database_url).await
}
