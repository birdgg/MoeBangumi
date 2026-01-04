use std::process::Command;

fn main() {
    // Try to get version from git tag
    let version = get_git_version().unwrap_or_else(|| env!("CARGO_PKG_VERSION").to_string());

    println!("cargo:rustc-env=APP_VERSION={}", version);

    // Rerun if git HEAD changes
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/refs/tags");
}

fn get_git_version() -> Option<String> {
    // Get the latest tag
    let output = Command::new("git")
        .args(["describe", "--tags", "--abbrev=0"])
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let tag = String::from_utf8(output.stdout).ok()?.trim().to_string();

    // Remove 'v' prefix if present
    let version = tag.strip_prefix('v').unwrap_or(&tag).to_string();

    Some(version)
}
