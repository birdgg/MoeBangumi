use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;
use transmission_rpc::{
    types::{
        BasicAuth, Id, Torrent, TorrentAddArgs, TorrentAddedOrDuplicate, TorrentGetField,
        TorrentSetArgs, TorrentStatus,
    },
    TransClient,
};

use crate::error::{DownloaderError, Result};
use crate::models::{AddTaskOptions, Task, TaskFile, TaskFilter, TaskStatus as UnifiedTaskStatus};
use crate::traits::Downloader;

/// Transmission downloader implementation
pub struct TransmissionDownloader {
    client: Arc<RwLock<TransClient>>,
}

impl TransmissionDownloader {
    /// Create a new Transmission downloader
    pub fn new(
        url: impl Into<String>,
        username: Option<String>,
        password: Option<String>,
    ) -> Result<Self> {
        let url_str = url.into();
        let parsed_url = url::Url::parse(&url_str)
            .map_err(|e| DownloaderError::Config(format!("Invalid URL: {}", e)))?;

        let client = match (&username, &password) {
            (Some(u), Some(p)) => {
                let auth = BasicAuth {
                    user: u.clone(),
                    password: p.clone(),
                };
                TransClient::with_auth(parsed_url, auth)
            }
            _ => TransClient::new(parsed_url),
        };

        Ok(Self {
            client: Arc::new(RwLock::new(client)),
        })
    }
}

// ============================================================================
// Type Conversions: Transmission -> Unified Models
// ============================================================================

/// Convert Transmission TorrentStatus to unified TaskStatus
fn parse_status(status: TorrentStatus) -> UnifiedTaskStatus {
    match status {
        TorrentStatus::Stopped => UnifiedTaskStatus::Paused,
        TorrentStatus::QueuedToVerify | TorrentStatus::Verifying => UnifiedTaskStatus::Checking,
        TorrentStatus::QueuedToDownload => UnifiedTaskStatus::Queued,
        TorrentStatus::Downloading => UnifiedTaskStatus::Downloading,
        TorrentStatus::QueuedToSeed | TorrentStatus::Seeding => UnifiedTaskStatus::Seeding,
    }
}

/// Convert Transmission Torrent to unified Task
impl From<Torrent> for Task {
    fn from(torrent: Torrent) -> Self {
        let status = torrent
            .status
            .map(parse_status)
            .unwrap_or(UnifiedTaskStatus::Unknown);

        let progress = torrent.percent_done.unwrap_or(0.0) as f64;

        // Join labels into comma-separated string
        let tags = torrent.labels.map(|l| l.join(",")).unwrap_or_default();

        Self {
            id: torrent.hash_string.unwrap_or_default(),
            name: torrent.name.unwrap_or_default(),
            status,
            progress,
            save_path: torrent.download_dir.unwrap_or_default(),
            total_size: torrent.total_size.unwrap_or(0),
            downloaded: torrent.downloaded_ever.unwrap_or(0) as i64,
            eta: torrent.eta.unwrap_or(-1),
            tags,
            category: None, // Transmission doesn't have categories
        }
    }
}

/// Convert Transmission File to unified TaskFile
fn convert_file(index: usize, file: &transmission_rpc::types::File) -> TaskFile {
    let progress = if file.length > 0 {
        file.bytes_completed as f64 / file.length as f64
    } else {
        0.0
    };

    TaskFile {
        index: index as i32,
        path: file.name.clone(),
        size: file.length,
        progress,
    }
}

/// Get required fields for torrent listing
fn get_list_fields() -> Vec<TorrentGetField> {
    vec![
        TorrentGetField::HashString,
        TorrentGetField::Name,
        TorrentGetField::Status,
        TorrentGetField::PercentDone,
        TorrentGetField::DownloadDir,
        TorrentGetField::TotalSize,
        TorrentGetField::DownloadedEver,
        TorrentGetField::Eta,
        TorrentGetField::Labels,
    ]
}

/// Get required fields for file listing
fn get_file_fields() -> Vec<TorrentGetField> {
    vec![TorrentGetField::Files, TorrentGetField::HashString]
}

/// Convert transmission-rpc error to DownloaderError
fn map_trans_err(e: Box<dyn std::error::Error + Send + Sync>) -> DownloaderError {
    DownloaderError::Transmission(e.to_string())
}

// ============================================================================
// Helper Functions for Label Management
// ============================================================================

/// Get existing labels for a torrent
async fn get_existing_labels(client: &mut TransClient, id: &str) -> Result<Vec<String>> {
    let response = client
        .torrent_get(
            Some(vec![TorrentGetField::Labels]),
            Some(vec![Id::Hash(id.to_string())]),
        )
        .await
        .map_err(map_trans_err)?;

    Ok(response
        .arguments
        .torrents
        .into_iter()
        .next()
        .and_then(|t| t.labels)
        .unwrap_or_default())
}

/// Set labels for a torrent
async fn set_labels(client: &mut TransClient, id: &str, labels: Vec<String>) -> Result<()> {
    let set_args = TorrentSetArgs::default().labels(labels);
    client
        .torrent_set(set_args, Some(vec![Id::Hash(id.to_string())]))
        .await
        .map_err(map_trans_err)?;
    Ok(())
}

// ============================================================================
// Downloader Trait Implementation
// ============================================================================

#[async_trait]
impl Downloader for TransmissionDownloader {
    async fn login(&self) -> Result<()> {
        // Transmission handles auth automatically with each request
        // We just verify connectivity by making a simple request
        // Note: session_get requires &mut self in transmission-rpc library
        let mut client = self.client.write().await;
        client
            .session_get()
            .await
            .map_err(|e| DownloaderError::Auth(format!("Failed to connect: {}", e)))?;
        tracing::debug!("Transmission connection verified");
        Ok(())
    }

    async fn is_login(&self) -> Result<bool> {
        // Note: session_get requires &mut self in transmission-rpc library
        let mut client = self.client.write().await;
        match client.session_get().await {
            Ok(_) => Ok(true),
            Err(_) => Ok(false),
        }
    }

    async fn add_task(&self, options: AddTaskOptions) -> Result<String> {
        // Note: Transmission doesn't support renaming on add, rename option is ignored
        let add_args = TorrentAddArgs {
            filename: Some(options.url.clone()),
            download_dir: options.save_path,
            labels: if options.tags.is_empty() {
                None
            } else {
                Some(options.tags)
            },
            ..Default::default()
        };

        let mut client = self.client.write().await;
        let response = client.torrent_add(add_args).await.map_err(map_trans_err)?;

        // Extract hash from response using pattern matching
        let hash = match response.arguments {
            TorrentAddedOrDuplicate::TorrentAdded(torrent) => {
                torrent.hash_string.unwrap_or_else(|| options.url.clone())
            }
            TorrentAddedOrDuplicate::TorrentDuplicate(torrent) => {
                torrent.hash_string.unwrap_or_else(|| options.url.clone())
            }
            TorrentAddedOrDuplicate::Error => {
                return Err(DownloaderError::Transmission("Failed to add torrent".into()));
            }
        };

        tracing::debug!("Added torrent with hash: {}", hash);
        Ok(hash)
    }

    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<()> {
        let transmission_ids: Vec<Id> = ids.iter().map(|s| Id::Hash(s.to_string())).collect();

        let mut client = self.client.write().await;
        client
            .torrent_remove(transmission_ids, delete_files)
            .await
            .map_err(map_trans_err)?;

        Ok(())
    }

    async fn get_tasks(&self, filter: Option<&TaskFilter>) -> Result<Vec<Task>> {
        let mut client = self.client.write().await;

        // Get specific torrents by ID if filter has ids
        let ids = filter.and_then(|f| {
            f.ids
                .as_ref()
                .map(|ids| ids.iter().map(|s| Id::Hash(s.clone())).collect::<Vec<_>>())
        });

        let response = client
            .torrent_get(Some(get_list_fields()), ids)
            .await
            .map_err(map_trans_err)?;

        let mut tasks: Vec<Task> = response
            .arguments
            .torrents
            .into_iter()
            .map(Task::from)
            .collect();

        // Apply additional filters that Transmission doesn't support natively
        if let Some(f) = filter {
            // Filter by status(es)
            if f.statuses.is_some() {
                tasks.retain(|t| f.matches_status(t.status));
            }

            // Filter by tag
            if let Some(tag) = &f.tag {
                tasks.retain(|t| t.has_tag(tag));
            }

            // Note: Transmission doesn't have categories, so we skip category filter
        }

        Ok(tasks)
    }

    async fn get_task_files(&self, id: &str) -> Result<Vec<TaskFile>> {
        let mut client = self.client.write().await;

        let response = client
            .torrent_get(
                Some(get_file_fields()),
                Some(vec![Id::Hash(id.to_string())]),
            )
            .await
            .map_err(map_trans_err)?;

        let torrent = response
            .arguments
            .torrents
            .into_iter()
            .next()
            .ok_or_else(|| DownloaderError::Transmission(format!("Torrent not found: {}", id)))?;

        let files = torrent
            .files
            .map(|files| {
                files
                    .iter()
                    .enumerate()
                    .map(|(i, f)| convert_file(i, f))
                    .collect()
            })
            .unwrap_or_default();

        Ok(files)
    }

    async fn add_tags(&self, id: &str, tags: &[&str]) -> Result<()> {
        let mut client = self.client.write().await;

        // Get existing labels and add new ones (avoiding duplicates)
        let mut existing_labels = get_existing_labels(&mut client, id).await?;

        for tag in tags {
            if !existing_labels.contains(&tag.to_string()) {
                existing_labels.push(tag.to_string());
            }
        }

        set_labels(&mut client, id, existing_labels).await
    }

    async fn remove_tags(&self, id: &str, tags: &[&str]) -> Result<()> {
        let mut client = self.client.write().await;

        // If tags is empty, remove all tags
        if tags.is_empty() {
            return set_labels(&mut client, id, vec![]).await;
        }

        // Get existing labels and remove specified ones
        let existing_labels = get_existing_labels(&mut client, id).await?;
        let tags_set: std::collections::HashSet<&str> = tags.iter().copied().collect();
        let new_labels: Vec<String> = existing_labels
            .into_iter()
            .filter(|l| !tags_set.contains(l.as_str()))
            .collect();

        set_labels(&mut client, id, new_labels).await
    }

    async fn rename_file(&self, id: &str, old_path: &str, new_path: &str) -> Result<()> {
        let mut client = self.client.write().await;

        // Transmission's torrent-rename-path API behavior:
        // - path: the current path relative to torrent's download directory
        // - name: the new name
        //
        // When renaming a file in the same directory, 'name' should be just the filename.
        // When moving to a different directory, 'name' should include the path.
        let old_path_obj = std::path::Path::new(old_path);
        let new_path_obj = std::path::Path::new(new_path);

        // Compare parent directories to determine if we're just renaming or moving
        let same_dir = old_path_obj.parent() == new_path_obj.parent();

        let name = if same_dir {
            // Same directory - only pass the new filename
            new_path_obj
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or(new_path)
                .to_string()
        } else {
            // Different directory - pass the full new path
            new_path.to_string()
        };

        client
            .torrent_rename_path(
                vec![Id::Hash(id.to_string())],
                old_path.to_string(),
                name,
            )
            .await
            .map_err(map_trans_err)?;

        Ok(())
    }
}
