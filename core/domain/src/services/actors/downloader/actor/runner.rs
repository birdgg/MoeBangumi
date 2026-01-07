use std::sync::Arc;

use downloader::{Downloader, DownloaderClient, DownloaderError};
use futures::future::BoxFuture;
use tokio::sync::mpsc;

use super::super::tags;
use super::messages::DownloaderMessage;
use super::state::DownloaderActorState;
use crate::services::SettingsService;

/// Actor 主循环
pub struct DownloaderActor {
    state: DownloaderActorState,
    receiver: mpsc::Receiver<DownloaderMessage>,
}

impl DownloaderActor {
    pub fn new(settings: Arc<SettingsService>, receiver: mpsc::Receiver<DownloaderMessage>) -> Self {
        Self {
            state: DownloaderActorState::new(settings),
            receiver,
        }
    }

    /// 运行 Actor 主循环
    pub async fn run(mut self) {
        while let Some(msg) = self.receiver.recv().await {
            self.handle_message(msg).await;
        }

        tracing::info!("Downloader actor stopped");
    }

    /// 处理单条消息
    async fn handle_message(&mut self, msg: DownloaderMessage) {
        match msg {
            DownloaderMessage::InvalidateClient => {
                self.state.invalidate();
            }

            DownloaderMessage::IsAvailable { reply } => {
                let result = self.state.ensure_client().await.is_ok();
                let _ = reply.send(result);
            }

            DownloaderMessage::AddTask { options, reply } => {
                let result = self.with_retry_add_task(options).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::GetTasks { filter, reply } => {
                let result = self.with_retry_get_tasks(filter).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::GetTaskFiles { hash, reply } => {
                let result = self.with_retry_get_task_files(&hash).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::DeleteTask {
                ids,
                delete_files,
                reply,
            } => {
                let result = self.with_retry_delete_task(&ids, delete_files).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::AddTags { id, tags, reply } => {
                let result = self.with_retry_add_tags(&id, &tags).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::RemoveTags { id, tags, reply } => {
                let result = self.with_retry_remove_tags(&id, &tags).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::RenameFile {
                id,
                old_path,
                new_path,
                reply,
            } => {
                let result = self.with_retry_rename_file(&id, &old_path, &new_path).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::GetRenamePendingTasks { reply } => {
                let result = self.with_retry_get_rename_pending_tasks().await;
                let _ = reply.send(result);
            }

            DownloaderMessage::CompleteRename { id, reply } => {
                let result = self.with_retry_complete_rename(&id).await;
                let _ = reply.send(result);
            }
        }
    }

    /// 通用重试 helper：确保客户端连接，执行操作，认证错误时重试
    async fn with_retry<T, F>(&mut self, operation: F) -> Result<T, DownloaderError>
    where
        F: for<'a> Fn(&'a DownloaderClient) -> BoxFuture<'a, Result<T, DownloaderError>>,
    {
        self.state.ensure_client().await?;

        let result = operation(self.state.client().unwrap()).await;

        match result {
            Ok(v) => Ok(v),
            Err(e) if e.is_auth_error() => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.state.reauthenticate().await?;
                operation(self.state.client().unwrap()).await
            }
            Err(e) => Err(e),
        }
    }

    /// 添加任务（带重试）
    ///
    /// 自动添加以下 tag：
    /// - `tags::MOE`: 标识由本应用添加的任务
    /// - `tags::RENAME`: 确保 RenameJob 能识别需要重命名的任务
    async fn with_retry_add_task(
        &mut self,
        options: downloader::AddTaskOptions,
    ) -> Result<String, DownloaderError> {
        let opts = options.add_tag(tags::MOE).add_tag(tags::RENAME);
        self.with_retry(|client| {
            let opts = opts.clone();
            Box::pin(async move { client.add_task(opts).await })
        })
        .await
    }

    /// 获取任务列表（带重试）
    async fn with_retry_get_tasks(
        &mut self,
        filter: Option<downloader::TaskFilter>,
    ) -> Result<Vec<downloader::Task>, DownloaderError> {
        self.with_retry(|client| {
            let filter = filter.clone();
            Box::pin(async move { client.get_tasks(filter.as_ref()).await })
        })
        .await
    }

    /// 获取任务文件（带重试）
    async fn with_retry_get_task_files(
        &mut self,
        hash: &str,
    ) -> Result<Vec<downloader::TaskFile>, DownloaderError> {
        let hash = hash.to_string();
        self.with_retry(|client| {
            let hash = hash.clone();
            Box::pin(async move { client.get_task_files(&hash).await })
        })
        .await
    }

    /// 删除任务（带重试）
    async fn with_retry_delete_task(
        &mut self,
        ids: &[String],
        delete_files: bool,
    ) -> Result<(), DownloaderError> {
        let ids = ids.to_vec();
        self.with_retry(|client| {
            let ids = ids.clone();
            Box::pin(async move {
                let id_refs: Vec<&str> = ids.iter().map(|s| s.as_str()).collect();
                client.delete_task(&id_refs, delete_files).await
            })
        })
        .await
    }

    /// 添加标签（带重试）
    async fn with_retry_add_tags(
        &mut self,
        id: &str,
        tags: &[String],
    ) -> Result<(), DownloaderError> {
        let id = id.to_string();
        let tags = tags.to_vec();
        self.with_retry(|client| {
            let id = id.clone();
            let tags = tags.clone();
            Box::pin(async move {
                let tag_refs: Vec<&str> = tags.iter().map(|s| s.as_str()).collect();
                client.add_tags(&id, &tag_refs).await
            })
        })
        .await
    }

    /// 移除标签（带重试）
    async fn with_retry_remove_tags(
        &mut self,
        id: &str,
        tags: &[String],
    ) -> Result<(), DownloaderError> {
        let id = id.to_string();
        let tags = tags.to_vec();
        self.with_retry(|client| {
            let id = id.clone();
            let tags = tags.clone();
            Box::pin(async move {
                let tag_refs: Vec<&str> = tags.iter().map(|s| s.as_str()).collect();
                client.remove_tags(&id, &tag_refs).await
            })
        })
        .await
    }

    /// 重命名文件（带重试）
    async fn with_retry_rename_file(
        &mut self,
        id: &str,
        old_path: &str,
        new_path: &str,
    ) -> Result<(), DownloaderError> {
        let id = id.to_string();
        let old_path = old_path.to_string();
        let new_path = new_path.to_string();
        self.with_retry(|client| {
            let id = id.clone();
            let old_path = old_path.clone();
            let new_path = new_path.clone();
            Box::pin(async move { client.rename_file(&id, &old_path, &new_path).await })
        })
        .await
    }

    /// 获取待重命名任务（带重试）
    ///
    /// 返回已完成/做种中且有 `tags::RENAME` 标签的任务。
    async fn with_retry_get_rename_pending_tasks(
        &mut self,
    ) -> Result<Vec<downloader::Task>, DownloaderError> {
        use downloader::{TaskFilter, TaskStatus};

        let filter = TaskFilter::new()
            .statuses([TaskStatus::Completed, TaskStatus::Seeding])
            .tag(tags::RENAME);

        self.with_retry(|client| {
            let filter = filter.clone();
            Box::pin(async move { client.get_tasks(Some(&filter)).await })
        })
        .await
    }

    /// 标记任务重命名完成（带重试）
    ///
    /// 移除 `tags::RENAME` 标签。
    async fn with_retry_complete_rename(&mut self, id: &str) -> Result<(), DownloaderError> {
        let id = id.to_string();
        self.with_retry(|client| {
            let id = id.clone();
            Box::pin(async move { client.remove_tags(&id, &[tags::RENAME]).await })
        })
        .await
    }
}
