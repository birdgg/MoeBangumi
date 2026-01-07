use downloader::{AddTaskOptions, DownloaderError, Task, TaskFile, TaskFilter};
use tokio::sync::oneshot;

/// Actor 消息类型
pub enum DownloaderMessage {
    /// 检查可用性
    IsAvailable {
        reply: oneshot::Sender<bool>,
    },

    /// 添加下载任务
    AddTask {
        options: AddTaskOptions,
        reply: oneshot::Sender<Result<String, DownloaderError>>,
    },

    /// 获取任务列表
    GetTasks {
        filter: Option<TaskFilter>,
        reply: oneshot::Sender<Result<Vec<Task>, DownloaderError>>,
    },

    /// 获取任务文件
    GetTaskFiles {
        hash: String,
        reply: oneshot::Sender<Result<Vec<TaskFile>, DownloaderError>>,
    },

    /// 删除任务
    DeleteTask {
        ids: Vec<String>,
        delete_files: bool,
        reply: oneshot::Sender<Result<(), DownloaderError>>,
    },

    /// 添加标签
    AddTags {
        id: String,
        tags: Vec<String>,
        reply: oneshot::Sender<Result<(), DownloaderError>>,
    },

    /// 移除标签
    RemoveTags {
        id: String,
        tags: Vec<String>,
        reply: oneshot::Sender<Result<(), DownloaderError>>,
    },

    /// 重命名文件
    RenameFile {
        id: String,
        old_path: String,
        new_path: String,
        reply: oneshot::Sender<Result<(), DownloaderError>>,
    },

    /// 获取待重命名任务（已完成且有 rename 标签）
    GetRenamePendingTasks {
        reply: oneshot::Sender<Result<Vec<Task>, DownloaderError>>,
    },

    /// 标记任务重命名完成（移除 rename 标签）
    CompleteRename {
        id: String,
        reply: oneshot::Sender<Result<(), DownloaderError>>,
    },

    /// 内部消息：设置变化，使客户端失效
    InvalidateClient,
}
