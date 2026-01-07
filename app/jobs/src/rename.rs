//! Rename Actor
//!
//! Periodically scans for completed downloads and renames them to Plex/Jellyfin compatible names.

use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio::time::MissedTickBehavior;

use domain::services::RenameService;

/// Rename interval (10 minutes)
const RENAME_INTERVAL: Duration = Duration::from_secs(600);

/// Message types for RenameActor
enum Message {
    Shutdown,
}

/// Handle for communicating with RenameActor
#[derive(Clone)]
pub struct RenameHandle {
    sender: mpsc::Sender<Message>,
}

impl RenameHandle {
    /// Signal the actor to shutdown
    pub async fn shutdown(&self) {
        let _ = self.sender.send(Message::Shutdown).await;
    }
}

/// Rename Actor
///
/// Runs a background task that processes file renames at regular intervals.
struct RenameActor {
    rename_service: Arc<RenameService>,
    receiver: mpsc::Receiver<Message>,
}

impl RenameActor {
    fn new(rename_service: Arc<RenameService>, receiver: mpsc::Receiver<Message>) -> Self {
        Self {
            rename_service,
            receiver,
        }
    }

    async fn run(mut self) {
        let mut timer = tokio::time::interval(RENAME_INTERVAL);
        timer.set_missed_tick_behavior(MissedTickBehavior::Skip);

        // Skip first tick (immediate)
        timer.tick().await;

        loop {
            tokio::select! {
                _ = timer.tick() => {
                    self.execute().await;
                }
                msg = self.receiver.recv() => {
                    match msg {
                        Some(Message::Shutdown) | None => {
                            tracing::info!("Rename actor stopped");
                            break;
                        }
                    }
                }
            }
        }
    }

    async fn execute(&self) {
        tracing::debug!("Rename job started");

        if let Err(e) = self.rename_service.process_all().await {
            tracing::error!("Rename job failed: {}", e);
        } else {
            tracing::debug!("Rename job completed");
        }
    }
}

/// Create and start the rename actor
pub fn create_rename_actor(rename_service: Arc<RenameService>) -> RenameHandle {
    let (sender, receiver) = mpsc::channel(8);

    let actor = RenameActor::new(rename_service, receiver);
    tokio::spawn(actor.run());

    RenameHandle { sender }
}
