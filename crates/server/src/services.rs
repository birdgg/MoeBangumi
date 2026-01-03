mod bangumi;
mod cache;
mod calendar;
mod downloader;
mod http_client;
mod log;
mod metadata;
mod notification;
mod poster;
mod rename;
mod rss_processing;
mod scheduler;
mod settings;
mod tracing_layer;
mod washing;

pub use bangumi::{BangumiError, BangumiService};
pub use cache::{CacheError, CacheService};
pub use metadata::{MetadataError, MetadataService};
pub use calendar::{CalendarError, CalendarService};
pub use downloader::{
    create_downloader_service, AddTaskOptions, Downloader, DownloaderClient, DownloaderConfig,
    DownloaderError, DownloaderHandle, DownloaderType, Task, TaskFile, TaskFilter, TaskStatus,
};
pub use http_client::{HttpClientError, HttpClientService};
pub use log::{LogError, LogService};
pub use notification::{create_notification_service, NotificationError, NotificationService};
pub use poster::{PosterError, PosterService};
pub use rename::{RenameError, RenameService};
pub use rss_processing::RssProcessingService;
pub use scheduler::{
    JobResult, LogCleanupJob, MetadataSyncJob, RenameJob, RssFetchJob, SchedulerJob,
    SchedulerService,
};
pub use settings::{SettingsError, SettingsService, SettingsWatcher};
pub use tracing_layer::{create_log_channel, start_log_writer, DatabaseLayer, LogReceiver};
pub use washing::{WashParams, WashingError, WashingService};
