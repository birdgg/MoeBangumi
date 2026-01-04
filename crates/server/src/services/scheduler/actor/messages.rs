use tokio::sync::oneshot;

/// Scheduler 错误类型
#[derive(Debug, thiserror::Error)]
pub enum SchedulerError {
    #[error("Job not found: {0}")]
    JobNotFound(String),

    #[error("Job is already running: {0}")]
    JobAlreadyRunning(String),

    #[error("Service unavailable")]
    ServiceUnavailable,
}

/// Job 状态信息
#[derive(Clone, Debug)]
pub struct JobStatus {
    pub name: &'static str,
    pub interval_secs: u64,
    pub is_running: bool,
}

/// Scheduler Actor 消息类型
pub enum SchedulerMessage {
    /// 手动触发指定 Job
    TriggerJob {
        job_name: String,
        reply: oneshot::Sender<Result<(), SchedulerError>>,
    },

    /// 获取所有 Job 的状态列表
    ListJobs {
        reply: oneshot::Sender<Vec<JobStatus>>,
    },

    /// 内部消息：定时器触发
    TimerTick { job_name: &'static str },

    /// 内部消息：Job 执行完成
    JobCompleted {
        job_name: &'static str,
        success: bool,
    },
}
