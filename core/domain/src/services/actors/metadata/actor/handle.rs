use tokio::sync::mpsc;

use super::messages::MetadataMessage;

/// Metadata Actor 的对外接口
///
/// 提供元数据同步和 Poster 下载的 fire-and-forget 接口。
/// 所有方法都是非阻塞的，任务提交后立即返回。
#[derive(Clone)]
pub struct MetadataHandle {
    sender: mpsc::Sender<MetadataMessage>,
}

impl MetadataHandle {
    pub fn new(sender: mpsc::Sender<MetadataMessage>) -> Self {
        Self { sender }
    }

    /// 下载单个 Poster (fire-and-forget)
    ///
    /// 任务提交后立即返回，下载在后台异步执行。
    /// 如果相同 URL 正在下载中，会自动跳过重复请求。
    pub fn download(&self, metadata_id: i64, url: String) {
        let sender = self.sender.clone();
        tokio::spawn(async move {
            let _ = sender
                .send(MetadataMessage::DownloadPoster { metadata_id, url })
                .await;
        });
    }

    /// 批量下载 Posters (fire-and-forget)
    ///
    /// 提交多个下载任务，每个任务独立执行。
    pub fn download_batch(&self, tasks: Vec<(i64, String)>) {
        if tasks.is_empty() {
            return;
        }
        let sender = self.sender.clone();
        tokio::spawn(async move {
            let _ = sender
                .send(MetadataMessage::DownloadPosterBatch { tasks })
                .await;
        });
    }

    /// 触发完整的海报同步 (fire-and-forget)
    ///
    /// 同步内容：
    /// - 下载远程 Poster URL 到本地
    ///
    /// 通常由内部定时器自动触发，也可手动调用。
    pub fn trigger_sync(&self) {
        let sender = self.sender.clone();
        tokio::spawn(async move {
            let _ = sender.send(MetadataMessage::TriggerSync).await;
        });
    }

    /// 优雅关闭 Actor
    ///
    /// 发送关闭信号并等待 Actor 处理完当前消息后停止。
    /// 这是一个阻塞操作，会等待关闭信号发送成功。
    ///
    /// # Note
    /// 调用此方法后，Actor 将不再接受新消息。
    /// 建议在应用关闭序列中调用此方法。
    pub async fn shutdown(&self) {
        tracing::info!("Sending shutdown signal to metadata actor");
        if let Err(e) = self.sender.send(MetadataMessage::Shutdown).await {
            tracing::warn!("Failed to send shutdown signal: {}", e);
        }
    }

    /// 检查 Actor 是否仍在运行
    ///
    /// 如果 channel 已关闭，返回 false。
    pub fn is_running(&self) -> bool {
        !self.sender.is_closed()
    }
}
