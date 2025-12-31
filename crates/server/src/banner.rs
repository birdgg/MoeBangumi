pub fn print_banner() {
    let version = env!("CARGO_PKG_VERSION");

    tracing::info!(r#" ███╗   ███╗ ██████╗ ███████╗"#);
    tracing::info!(r#" ████╗ ████║██╔═══██╗██╔════╝    moe-bangumi"#);
    tracing::info!(r#" ██╔████╔██║██║   ██║█████╗      v{}"#, version);
    tracing::info!(r#" ██║╚██╔╝██║██║   ██║██╔══╝  "#);
    tracing::info!(r#" ██║ ╚═╝ ██║╚██████╔╝███████╗"#);
    tracing::info!(r#" ╚═╝     ╚═╝ ╚═════╝ ╚══════╝"#);
}
