pub fn print_banner(version: &str) {
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
