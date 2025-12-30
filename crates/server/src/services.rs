mod bangumi;
mod cache;
mod downloader;
mod http_client;
mod log;
mod notification;
mod poster;
mod rename;
mod rss_processing;
mod scheduler;
mod settings;
mod torrent_search;
mod tracing_layer;

pub use bangumi::{BangumiError, BangumiService};
pub use cache::{CacheError, CacheService};
pub use downloader::{
    AddTaskOptions, Downloader, DownloaderClient, DownloaderConfig, DownloaderError,
    DownloaderService, DownloaderType, Task, TaskFile, TaskFilter, TaskStatus,
};
pub use http_client::{HttpClientError, HttpClientService};
pub use log::{LogError, LogService};
pub use notification::{NotificationError, NotificationService};
pub use poster::{PosterError, PosterService};
pub use rename::{RenameError, RenameService};
pub use rss_processing::RssProcessingService;
pub use scheduler::{
    JobResult, LogCleanupJob, RenameJob, RssFetchJob, SchedulerJob, SchedulerService,
};
pub use settings::{SettingsError, SettingsService, SettingsWatcher};
pub use torrent_search::{TorrentSearchError, TorrentSearchService};
pub use tracing_layer::{create_log_channel, start_log_writer, DatabaseLayer, LogReceiver};
