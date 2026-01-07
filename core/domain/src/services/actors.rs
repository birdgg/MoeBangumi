//! Actor-based services
//!
//! Contains core actors that use the actor pattern for concurrency.
//! Timer-based actors (rss_fetch, rename, log_cleanup) are in the `jobs` crate.

pub mod downloader;
pub mod metadata;
pub mod notification;

// Downloader actor
pub use downloader::{
    create_downloader_service, AddTaskOptions, Downloader, DownloaderClient, DownloaderConfig,
    DownloaderError, DownloaderHandle, DownloaderType, Task, TaskFile, TaskFilter, TaskStatus,
};

// Metadata actor
pub use metadata::{
    create_metadata_actor, MetadataError, MetadataHandle, MetadataService, PosterError,
    PosterService,
};

// Notification actor
pub use notification::{
    create_notification_service, NotificationConfig, NotificationError, NotificationHandle,
    NotificationService, Notifier, Topic,
};
