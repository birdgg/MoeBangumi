use tokio::sync::{mpsc, oneshot};

use super::messages::{JobStatus, SchedulerError, SchedulerMessage};

/// Scheduler Actor 的对外接口
///
/// 通过 channel 与 Actor 通信，提供任务调度管理功能。
#[derive(Clone)]
pub struct SchedulerHandle {
    sender: mpsc::Sender<SchedulerMessage>,
}

impl SchedulerHandle {
    pub fn new(sender: mpsc::Sender<SchedulerMessage>) -> Self {
        Self { sender }
    }

    /// 手动触发指定 Job
    ///
    /// # Arguments
    /// * `job_name` - Job 的名称（如 "RssFetch", "FileRename" 等）
    ///
    /// # Returns
    /// * `Ok(())` - Job 已成功开始执行
    /// * `Err(SchedulerError::JobNotFound)` - 未找到指定名称的 Job
    /// * `Err(SchedulerError::JobAlreadyRunning)` - Job 正在执行中
    pub async fn trigger_job(&self, job_name: &str) -> Result<(), SchedulerError> {
        let (reply, rx) = oneshot::channel();
        self.sender
            .send(SchedulerMessage::TriggerJob {
                job_name: job_name.to_string(),
                reply,
            })
            .await
            .map_err(|_| SchedulerError::ServiceUnavailable)?;
        rx.await.map_err(|_| SchedulerError::ServiceUnavailable)?
    }

    /// 获取所有 Job 的状态
    pub async fn list_jobs(&self) -> Result<Vec<JobStatus>, SchedulerError> {
        let (reply, rx) = oneshot::channel();
        self.sender
            .send(SchedulerMessage::ListJobs { reply })
            .await
            .map_err(|_| SchedulerError::ServiceUnavailable)?;
        rx.await.map_err(|_| SchedulerError::ServiceUnavailable)
    }

    /// 发送定时器触发消息（内部使用）
    pub(super) async fn send_timer_tick(&self, job_name: &'static str) {
        let _ = self
            .sender
            .send(SchedulerMessage::TimerTick { job_name })
            .await;
    }

    /// 发送 Job 完成消息（内部使用）
    pub(super) async fn send_job_completed(&self, job_name: &'static str, success: bool) {
        let _ = self
            .sender
            .send(SchedulerMessage::JobCompleted { job_name, success })
            .await;
    }
}
