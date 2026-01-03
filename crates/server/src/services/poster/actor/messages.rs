/// Poster 下载任务
#[derive(Debug, Clone)]
pub struct DownloadTask {
    /// Metadata 记录 ID
    pub metadata_id: i64,
    /// 图片 URL
    pub url: String,
}

/// Actor 消息类型
pub enum PosterDownloadMessage {
    /// 提交下载任务（fire-and-forget）
    Download(DownloadTask),
}
