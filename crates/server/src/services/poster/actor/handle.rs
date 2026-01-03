use tokio::sync::mpsc;

use super::messages::{DownloadTask, PosterDownloadMessage};

/// Poster 下载 Actor 的对外接口
///
/// 通过 channel 与 Actor 通信，提供异步图片下载功能。
/// 所有方法都是 fire-and-forget，立即返回不等待下载完成。
#[derive(Clone)]
pub struct PosterDownloadHandle {
    sender: mpsc::Sender<PosterDownloadMessage>,
}

impl PosterDownloadHandle {
    pub fn new(sender: mpsc::Sender<PosterDownloadMessage>) -> Self {
        Self { sender }
    }

    /// 提交单个下载任务（fire-and-forget，立即返回）
    ///
    /// # Arguments
    /// * `metadata_id` - Metadata 记录 ID，用于更新 poster_url
    /// * `url` - 图片 URL
    pub fn download(&self, metadata_id: i64, url: String) {
        let sender = self.sender.clone();
        let task = DownloadTask { metadata_id, url };
        tokio::spawn(async move {
            let _ = sender.send(PosterDownloadMessage::Download(task)).await;
        });
    }

    /// 批量提交下载任务（fire-and-forget，立即返回）
    ///
    /// # Arguments
    /// * `tasks` - (metadata_id, url) 元组列表
    pub fn download_batch(&self, tasks: Vec<(i64, String)>) {
        // Early return to avoid unnecessary loop
        if tasks.is_empty() {
            return;
        }
        for (metadata_id, url) in tasks {
            self.download(metadata_id, url);
        }
    }
}
