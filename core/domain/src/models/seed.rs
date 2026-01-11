use serde::{Deserialize, Serialize};

use super::CreateBangumi;

/// Data for a single season (used for seed data import/export)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeasonData {
    pub year: i32,
    pub season: String,
    pub entries: Vec<CreateBangumi>,
}
