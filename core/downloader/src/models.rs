mod add_task_options;
mod task;
mod task_file;
mod task_filter;
mod task_status;

pub use add_task_options::AddTaskOptions;
pub use task::Task;
pub use task_file::TaskFile;
pub use task_filter::TaskFilter;
pub use task_status::TaskStatus;

use serde::{Deserialize, Serialize};
/// Downloader type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum DownloaderType {
    #[default]
    #[serde(rename = "qBittorrent")]
    QBittorrent,
    #[serde(rename = "Transmission")]
    Transmission,
}
