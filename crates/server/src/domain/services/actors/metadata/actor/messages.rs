/// Metadata Actor 消息类型
pub enum MetadataMessage {
    /// 下载单个 Poster (fire-and-forget)
    DownloadPoster { metadata_id: i64, url: String },

    /// 批量下载 Posters (fire-and-forget)
    DownloadPosterBatch { tasks: Vec<(i64, String)> },

    /// 触发完整的元数据同步 (fire-and-forget)
    TriggerSync,

    /// 优雅关闭 Actor
    Shutdown,
}

/// 同步结果统计
#[derive(Debug, Clone, Default)]
pub struct SyncStats {
    pub posters_queued: usize,
}
