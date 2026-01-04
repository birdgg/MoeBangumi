use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::mpsc;

use super::handle::SchedulerHandle;
use super::messages::{JobStatus, SchedulerError, SchedulerMessage};
use crate::services::scheduler::traits::SchedulerJob;

/// 单个 Job 的运行时状态
struct JobEntry {
    job: Arc<dyn SchedulerJob>,
    is_running: bool,
}

/// Scheduler Actor
///
/// 使用非阻塞方式执行 Job：
/// - Job 在独立的 tokio 任务中运行
/// - Actor 主循环保持响应性
/// - 支持多个 Job 并行执行
pub struct SchedulerActor {
    jobs: HashMap<&'static str, JobEntry>,
    receiver: mpsc::Receiver<SchedulerMessage>,
    handle: SchedulerHandle,
}

impl SchedulerActor {
    pub fn new(
        jobs: Vec<Arc<dyn SchedulerJob>>,
        receiver: mpsc::Receiver<SchedulerMessage>,
        handle: SchedulerHandle,
    ) -> Self {
        let mut job_map = HashMap::new();

        for job in jobs {
            let name = job.name();
            job_map.insert(
                name,
                JobEntry {
                    job,
                    is_running: false,
                },
            );
        }

        Self {
            jobs: job_map,
            receiver,
            handle,
        }
    }

    /// 启动各 Job 的定时任务
    pub fn spawn_timers(&self) {
        for (name, entry) in &self.jobs {
            let handle = self.handle.clone();
            let interval = entry.job.interval();
            let job_name = *name;

            tokio::spawn(async move {
                let mut timer = tokio::time::interval(interval);
                timer.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

                loop {
                    timer.tick().await;
                    handle.send_timer_tick(job_name).await;
                }
            });
        }
    }

    /// 运行 Actor 主循环
    pub async fn run(mut self) {
        tracing::info!("Scheduler actor started with {} jobs", self.jobs.len());

        while let Some(msg) = self.receiver.recv().await {
            self.handle_message(msg);
        }

        tracing::info!("Scheduler actor stopped");
    }

    /// 处理消息
    fn handle_message(&mut self, msg: SchedulerMessage) {
        match msg {
            SchedulerMessage::TriggerJob { job_name, reply } => {
                let result = self.trigger_job_by_name(&job_name);
                let _ = reply.send(result);
            }

            SchedulerMessage::ListJobs { reply } => {
                let statuses = self.get_job_statuses();
                let _ = reply.send(statuses);
            }

            SchedulerMessage::TimerTick { job_name } => {
                self.spawn_job(job_name);
            }

            SchedulerMessage::JobCompleted { job_name, success } => {
                self.handle_job_completed(job_name, success);
            }
        }
    }

    /// 处理 Job 完成消息
    fn handle_job_completed(&mut self, job_name: &'static str, success: bool) {
        if let Some(entry) = self.jobs.get_mut(job_name) {
            entry.is_running = false;
        }

        if success {
            tracing::debug!("Job '{}' completed successfully", job_name);
        } else {
            tracing::error!("Job '{}' failed", job_name);
        }
    }

    /// 按名称触发 Job
    fn trigger_job_by_name(&mut self, job_name: &str) -> Result<(), SchedulerError> {
        // 查找 Job
        let entry = self
            .jobs
            .iter()
            .find(|(name, _)| **name == job_name)
            .map(|(name, _)| *name);

        match entry {
            Some(name) => {
                let job_entry = self.jobs.get(name).unwrap();
                if job_entry.is_running {
                    Err(SchedulerError::JobAlreadyRunning(job_name.to_string()))
                } else {
                    self.spawn_job(name);
                    Ok(())
                }
            }
            None => Err(SchedulerError::JobNotFound(job_name.to_string())),
        }
    }

    /// 非阻塞地启动 Job 执行
    ///
    /// Job 在独立的 tokio 任务中执行，完成后通过 JobCompleted 消息通知 Actor。
    fn spawn_job(&mut self, name: &'static str) {
        let entry = match self.jobs.get_mut(name) {
            Some(e) => e,
            None => return,
        };

        // 检查是否正在运行
        if entry.is_running {
            tracing::debug!("Job '{}' is already running, skipping this trigger", name);
            return;
        }

        // 标记为运行中
        entry.is_running = true;
        let job = Arc::clone(&entry.job);
        let handle = self.handle.clone();

        // 在独立任务中执行 Job
        tokio::spawn(async move {
            let result = job.execute().await;
            let success = result.is_ok();

            if let Err(e) = &result {
                tracing::error!("Job '{}' execution error: {}", name, e);
            }

            // 通知 Actor 执行完成
            handle.send_job_completed(name, success).await;
        });
    }

    /// 获取所有 Job 状态
    fn get_job_statuses(&self) -> Vec<JobStatus> {
        self.jobs
            .iter()
            .map(|(name, entry)| JobStatus {
                name,
                interval_secs: entry.job.interval().as_secs(),
                is_running: entry.is_running,
            })
            .collect()
    }
}
