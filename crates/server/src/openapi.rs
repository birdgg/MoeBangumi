use utoipa::OpenApi;

use crate::models::{CreateTodo, Todo};

#[derive(OpenApi)]
#[openapi(
    info(
        title = "Todo API",
        version = "1.0.0",
        description = "A simple Todo REST API built with Axum"
    ),
    tags(
        (name = "todos", description = "Todo management endpoints")
    ),
    components(schemas(Todo, CreateTodo))
)]
pub struct ApiDoc;
