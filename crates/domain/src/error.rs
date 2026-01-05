//! Domain layer errors.
//!
//! This module defines errors that can occur within the domain layer,
//! representing business rule violations and entity-level errors.

use thiserror::Error;

/// Domain layer error type.
#[derive(Debug, Error)]
pub enum DomainError {
    /// Entity not found.
    #[error("Entity not found: {entity_type} with id {id}")]
    NotFound {
        entity_type: &'static str,
        id: String,
    },

    /// Validation error for entity fields.
    #[error("Validation error: {0}")]
    Validation(String),

    /// Business rule violation.
    #[error("Business rule violation: {0}")]
    BusinessRule(String),

    /// Persistence layer error (abstracted).
    #[error("Persistence error: {0}")]
    Persistence(String),
}

/// Result type alias for domain operations.
pub type DomainResult<T> = Result<T, DomainError>;
