use super::TaskStatus;

/// Filter options for querying tasks.
#[derive(Debug, Clone, Default)]
pub struct TaskFilter {
    /// Filter by statuses (matches any of the specified statuses)
    pub statuses: Option<Vec<TaskStatus>>,

    /// Filter by category
    pub category: Option<String>,

    /// Filter by tag
    pub tag: Option<String>,

    /// Filter by specific IDs
    pub ids: Option<Vec<String>>,
}

impl TaskFilter {
    /// Create a new empty filter
    pub fn new() -> Self {
        Self::default()
    }

    /// Set single status filter (builder pattern)
    pub fn status(mut self, status: TaskStatus) -> Self {
        self.statuses = Some(vec![status]);
        self
    }

    /// Set multiple statuses filter (builder pattern)
    /// Matches tasks that have any of the specified statuses
    pub fn statuses(mut self, statuses: impl IntoIterator<Item = TaskStatus>) -> Self {
        self.statuses = Some(statuses.into_iter().collect());
        self
    }

    /// Set category filter (builder pattern)
    pub fn category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    /// Set tag filter (builder pattern)
    pub fn tag(mut self, tag: impl Into<String>) -> Self {
        self.tag = Some(tag.into());
        self
    }

    /// Set IDs filter (builder pattern)
    pub fn ids(mut self, ids: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.ids = Some(ids.into_iter().map(Into::into).collect());
        self
    }

    /// Set single ID filter (builder pattern)
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.ids = Some(vec![id.into()]);
        self
    }

    /// Check if a status matches the filter
    pub fn matches_status(&self, status: TaskStatus) -> bool {
        match &self.statuses {
            Some(statuses) => statuses.contains(&status),
            None => true,
        }
    }
}
