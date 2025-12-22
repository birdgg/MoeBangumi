use async_trait::async_trait;
use sqlx::{sqlite::SqlitePoolOptions, SqlitePool};

use crate::models::{CreateTodo, Todo};

/// TodoRepository trait - 数据访问抽象层
#[async_trait]
pub trait TodoRepository: Send + Sync {
    async fn list_todos(&self) -> Result<Vec<Todo>, sqlx::Error>;
    async fn create_todo(&self, input: &CreateTodo) -> Result<Todo, sqlx::Error>;
    async fn delete_todo(&self, id: i64) -> Result<bool, sqlx::Error>;
}

/// 为 SqlitePool 实现 TodoRepository trait
#[async_trait]
impl TodoRepository for SqlitePool {
    async fn list_todos(&self) -> Result<Vec<Todo>, sqlx::Error> {
        sqlx::query_as::<_, Todo>(
            "SELECT id, title, completed, created_at FROM todos ORDER BY id DESC",
        )
        .fetch_all(self)
        .await
    }

    async fn create_todo(&self, input: &CreateTodo) -> Result<Todo, sqlx::Error> {
        let result = sqlx::query("INSERT INTO todos (title) VALUES (?)")
            .bind(&input.title)
            .execute(self)
            .await?;

        let id = result.last_insert_rowid();

        sqlx::query_as::<_, Todo>(
            "SELECT id, title, completed, created_at FROM todos WHERE id = ?",
        )
        .bind(id)
        .fetch_one(self)
        .await
    }

    async fn delete_todo(&self, id: i64) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("DELETE FROM todos WHERE id = ?")
            .bind(id)
            .execute(self)
            .await?;

        Ok(result.rows_affected() > 0)
    }
}

pub async fn create_pool(database_url: &str) -> Result<SqlitePool, sqlx::Error> {
    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect(database_url)
        .await?;

    // Run migrations
    sqlx::query(
        r#"
        CREATE TABLE IF NOT EXISTS todos (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            title TEXT NOT NULL,
            completed BOOLEAN NOT NULL DEFAULT FALSE,
            created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
        )
        "#,
    )
    .execute(&pool)
    .await?;

    Ok(pool)
}
