pub fn print_banner() {
    let version = env!("CARGO_PKG_VERSION");

    let banner = format!(
        r#"
 ███╗   ███╗ ██████╗ ███████╗
 ████╗ ████║██╔═══██╗██╔════╝    moe-bangumi
 ██╔████╔██║██║   ██║█████╗      v{}
 ██║╚██╔╝██║██║   ██║██╔══╝
 ██║ ╚═╝ ██║╚██████╔╝███████╗
 ╚═╝     ╚═╝ ╚═════╝ ╚══════╝
"#,
        version
    );

    tracing::info!("{}", banner);
}
