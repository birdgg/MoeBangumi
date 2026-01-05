use async_trait::async_trait;
use std::time::Duration;

/// Result type for scheduler job execution.
pub type JobResult = Result<(), Box<dyn std::error::Error + Send + Sync>>;

/// Trait for defining a scheduled job.
///
/// Each scheduled job must implement this trait to define its execution logic
/// and scheduling interval. Jobs are executed periodically by the [`SchedulerService`].
///
/// # Example
///
/// ```rust,ignore
/// use async_trait::async_trait;
/// use std::time::Duration;
/// use crate::services::scheduler::{SchedulerJob, JobResult};
///
/// pub struct MyJob;
///
/// #[async_trait]
/// impl SchedulerJob for MyJob {
///     fn name(&self) -> &'static str {
///         "MyJob"
///     }
///
///     fn interval(&self) -> Duration {
///         Duration::from_secs(60)
///     }
///
///     async fn execute(&self) -> JobResult {
///         // Job logic here
///         Ok(())
///     }
/// }
/// ```
#[async_trait]
pub trait SchedulerJob: Send + Sync {
    /// Returns the unique name of this job.
    ///
    /// Used for logging and identification purposes.
    fn name(&self) -> &'static str;

    /// Returns the interval between job executions.
    ///
    /// The scheduler will wait this duration between each execution.
    fn interval(&self) -> Duration;

    /// Executes the job logic.
    ///
    /// This method is called periodically by the scheduler.
    /// Errors are logged but do not stop the scheduler.
    ///
    /// # Returns
    ///
    /// - `Ok(())` if the job executed successfully
    /// - `Err(e)` if the job failed (will be logged and retried next interval)
    async fn execute(&self) -> JobResult;
}
