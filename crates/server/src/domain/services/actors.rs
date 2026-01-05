//! Actor-based services
//!
//! Contains all services that use the actor pattern for concurrency.

pub mod downloader;
pub mod metadata;
pub mod notification;
pub mod scheduler;

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

// Scheduler actor
pub use scheduler::{
    JobResult, JobStatus, LogCleanupJob, RenameJob, RssFetchJob, SchedulerBuilder, SchedulerError,
    SchedulerHandle, SchedulerJob, SchedulerService,
};
