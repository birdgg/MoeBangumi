mod calendar_refresh_job;
mod log_cleanup_job;
mod rename_job;
mod rss_fetch_job;
mod traits;

pub use calendar_refresh_job::CalendarRefreshJob;
pub use log_cleanup_job::LogCleanupJob;
pub use rename_job::RenameJob;
pub use rss_fetch_job::RssFetchJob;
pub use traits::{JobResult, SchedulerJob};

use std::sync::Arc;

/// Scheduler service that manages periodic background tasks.
///
/// The scheduler runs registered jobs at their specified intervals.
/// Each job runs independently in its own tokio task.
///
/// # Example
///
/// ```rust,ignore
/// let scheduler = SchedulerService::new()
///     .with_job(RssFetchJob::new());
///
/// scheduler.start();
/// ```
pub struct SchedulerService {
    jobs: Vec<Arc<dyn SchedulerJob>>,
}

impl SchedulerService {
    /// Creates a new scheduler service with no jobs.
    pub fn new() -> Self {
        Self { jobs: Vec::new() }
    }

    /// Adds a job to the scheduler.
    ///
    /// Jobs are not started until [`start`](Self::start) is called.
    pub fn with_job<J: SchedulerJob + 'static>(mut self, job: J) -> Self {
        self.jobs.push(Arc::new(job));
        self
    }

    /// Adds an already-wrapped Arc job to the scheduler.
    ///
    /// This is useful when you need to keep a reference to the job for manual triggering.
    pub fn with_arc_job<J: SchedulerJob + 'static>(mut self, job: Arc<J>) -> Self {
        self.jobs.push(job);
        self
    }

    /// Starts all registered jobs.
    ///
    /// Each job runs in its own tokio task and executes at its specified interval.
    /// This method returns immediately after spawning all tasks.
    pub fn start(&self) {
        for job in &self.jobs {
            let job = Arc::clone(job);
            tokio::spawn(async move {
                Self::run_job_loop(job).await;
            });
        }
    }

    /// Runs a single job in an infinite loop.
    async fn run_job_loop(job: Arc<dyn SchedulerJob>) {
        let name = job.name();
        let interval = job.interval();

        let mut timer = tokio::time::interval(interval);
        timer.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

        loop {
            timer.tick().await;

            match job.execute().await {
                Ok(()) => {
                    tracing::debug!("Job '{}' completed successfully", name);
                }
                Err(e) => {
                    tracing::error!("Job '{}' failed: {}", name, e);
                }
            }
        }
    }

    /// Returns the number of registered jobs.
    pub fn job_count(&self) -> usize {
        self.jobs.len()
    }
}

impl Default for SchedulerService {
    fn default() -> Self {
        Self::new()
    }
}
