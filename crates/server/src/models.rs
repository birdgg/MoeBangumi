use serde::{Deserialize, Serialize};
use sqlx::FromRow;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Todo {
    pub id: i64,
    pub title: String,
    pub completed: bool,
    pub created_at: String,
}

#[derive(Debug, Deserialize)]
pub struct CreateTodo {
    pub title: String,
}
