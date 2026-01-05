mod actor;
mod jobs;
mod traits;

pub use actor::{JobStatus, SchedulerError, SchedulerHandle};
pub use jobs::{LogCleanupJob, RenameJob, RssFetchJob};
pub use traits::{JobResult, SchedulerJob};

use actor::{SchedulerActor, SchedulerMessage};
use std::sync::Arc;
use tokio::sync::mpsc;

/// Scheduler service 类型别名，保持 API 兼容性
pub type SchedulerService = SchedulerHandle;

/// Scheduler 构建器
///
/// 使用 builder 模式创建 Scheduler Actor。
///
/// # Example
///
/// ```rust,ignore
/// let scheduler = SchedulerBuilder::new()
///     .with_job(RssFetchJob::new(...))
///     .with_job(LogCleanupJob::new(...))
///     .build();
///
/// // 手动触发 Job
/// scheduler.trigger_job("RssFetch").await?;
/// ```
pub struct SchedulerBuilder {
    jobs: Vec<Arc<dyn SchedulerJob>>,
}

impl SchedulerBuilder {
    /// 创建新的 SchedulerBuilder
    pub fn new() -> Self {
        Self { jobs: Vec::new() }
    }

    /// 添加 Job 到调度器
    pub fn with_job<J: SchedulerJob + 'static>(mut self, job: J) -> Self {
        self.jobs.push(Arc::new(job));
        self
    }

    /// 构建并启动 Scheduler Actor
    ///
    /// 返回 SchedulerHandle，可用于手动触发 Job 和查询状态。
    pub fn build(self) -> SchedulerHandle {
        let (sender, receiver) = mpsc::channel::<SchedulerMessage>(32);
        let handle = SchedulerHandle::new(sender);

        let actor = SchedulerActor::new(self.jobs, receiver, handle.clone());

        // 启动各 Job 的定时任务
        actor.spawn_timers();

        // 启动 Actor 主循环
        tokio::spawn(actor.run());

        handle
    }
}

impl Default for SchedulerBuilder {
    fn default() -> Self {
        Self::new()
    }
}
