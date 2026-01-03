use downloader::{AddTaskOptions, DownloaderError, Task, TaskFile, TaskFilter};
use tokio::sync::{mpsc, oneshot};

use super::messages::DownloaderMessage;

/// Downloader Actor 的对外接口
///
/// 通过 channel 与 Actor 通信，提供下载任务管理功能。
#[derive(Clone)]
pub struct DownloaderHandle {
    sender: mpsc::Sender<DownloaderMessage>,
}

impl DownloaderHandle {
    pub fn new(sender: mpsc::Sender<DownloaderMessage>) -> Self {
        Self { sender }
    }

    /// 发送消息并等待响应
    async fn send_and_recv<T>(
        &self,
        msg_fn: impl FnOnce(oneshot::Sender<Result<T, DownloaderError>>) -> DownloaderMessage,
    ) -> Result<T, DownloaderError> {
        let (reply, rx) = oneshot::channel();
        self.sender
            .send(msg_fn(reply))
            .await
            .map_err(|_| DownloaderError::ServiceUnavailable)?;
        rx.await.map_err(|_| DownloaderError::ServiceUnavailable)?
    }

    /// 检查 downloader 是否可用
    pub async fn is_available(&self) -> bool {
        let (reply, rx) = oneshot::channel();
        if self
            .sender
            .send(DownloaderMessage::IsAvailable { reply })
            .await
            .is_err()
        {
            return false;
        }
        rx.await.unwrap_or(false)
    }

    /// 添加下载任务
    pub async fn add_task(&self, options: AddTaskOptions) -> Result<String, DownloaderError> {
        self.send_and_recv(|reply| DownloaderMessage::AddTask { options, reply })
            .await
    }

    /// 获取任务文件
    pub async fn get_task_files(&self, hash: &str) -> Result<Vec<TaskFile>, DownloaderError> {
        let hash = hash.to_string();
        self.send_and_recv(|reply| DownloaderMessage::GetTaskFiles { hash, reply })
            .await
    }

    /// 获取任务列表
    pub async fn get_tasks(
        &self,
        filter: Option<&TaskFilter>,
    ) -> Result<Vec<Task>, DownloaderError> {
        let filter = filter.cloned();
        self.send_and_recv(|reply| DownloaderMessage::GetTasks { filter, reply })
            .await
    }

    /// 删除任务
    pub async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<(), DownloaderError> {
        let ids: Vec<String> = ids.iter().map(|s| s.to_string()).collect();
        self.send_and_recv(|reply| DownloaderMessage::DeleteTask {
            ids,
            delete_files,
            reply,
        })
        .await
    }

    /// 添加标签
    pub async fn add_tags(&self, id: &str, tags: &[&str]) -> Result<(), DownloaderError> {
        let id = id.to_string();
        let tags: Vec<String> = tags.iter().map(|s| s.to_string()).collect();
        self.send_and_recv(|reply| DownloaderMessage::AddTags { id, tags, reply })
            .await
    }

    /// 移除标签
    pub async fn remove_tags(&self, id: &str, tags: &[&str]) -> Result<(), DownloaderError> {
        let id = id.to_string();
        let tags: Vec<String> = tags.iter().map(|s| s.to_string()).collect();
        self.send_and_recv(|reply| DownloaderMessage::RemoveTags { id, tags, reply })
            .await
    }

    /// 重命名文件
    pub async fn rename_file(
        &self,
        id: &str,
        old_path: &str,
        new_path: &str,
    ) -> Result<(), DownloaderError> {
        let id = id.to_string();
        let old_path = old_path.to_string();
        let new_path = new_path.to_string();
        self.send_and_recv(|reply| DownloaderMessage::RenameFile {
            id,
            old_path,
            new_path,
            reply,
        })
        .await
    }

    /// 获取待重命名任务
    ///
    /// 返回已完成/做种中且有 rename 标签的任务。
    pub async fn get_rename_pending_tasks(&self) -> Result<Vec<Task>, DownloaderError> {
        self.send_and_recv(|reply| DownloaderMessage::GetRenamePendingTasks { reply })
            .await
    }

    /// 标记任务重命名完成
    ///
    /// 移除 rename 标签，表示该任务已完成重命名处理。
    pub async fn complete_rename(&self, id: &str) -> Result<(), DownloaderError> {
        let id = id.to_string();
        self.send_and_recv(|reply| DownloaderMessage::CompleteRename { id, reply })
            .await
    }

    /// 发送失效通知（内部使用）
    pub(crate) async fn invalidate(&self) {
        let _ = self.sender.send(DownloaderMessage::InvalidateClient).await;
    }
}
