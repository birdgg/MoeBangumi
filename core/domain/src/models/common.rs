use serde::{Deserialize, Deserializer, Serialize};

/// Wrapper for optional fields that can be explicitly cleared.
/// - `Unchanged`: Field was not provided in the request, keep existing value
/// - `Clear`: Field was explicitly set to null, clear the value
/// - `Set(T)`: Field was set to a new value
///
/// In JSON: null means Clear, value means Set(value), absent means Unchanged
#[derive(Debug, Clone, Default)]
pub enum Clearable<T> {
    #[default]
    Unchanged,
    Clear,
    Set(T),
}

impl<T> Clearable<T> {
    pub fn resolve(self, existing: Option<T>) -> Option<T> {
        match self {
            Clearable::Unchanged => existing,
            Clearable::Clear => None,
            Clearable::Set(v) => Some(v),
        }
    }

    /// Returns true if this field should be updated (Clear or Set)
    pub fn should_update(&self) -> bool {
        !matches!(self, Clearable::Unchanged)
    }

    /// Returns the value to set, or None for Clear/Unchanged
    pub fn into_value(self) -> Option<T> {
        match self {
            Clearable::Set(v) => Some(v),
            _ => None,
        }
    }
}

impl Clearable<String> {
    /// Resolve to String, where Clear means empty string
    pub fn resolve_or_empty(self, existing: String) -> String {
        match self {
            Clearable::Unchanged => existing,
            Clearable::Clear => String::new(),
            Clearable::Set(v) => v,
        }
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for Clearable<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let opt = Option::<T>::deserialize(deserializer)?;
        Ok(match opt {
            Some(v) => Clearable::Set(v),
            None => Clearable::Clear,
        })
    }
}

impl<T: Serialize> Serialize for Clearable<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Clearable::Unchanged => serializer.serialize_none(),
            Clearable::Clear => serializer.serialize_none(),
            Clearable::Set(v) => v.serialize(serializer),
        }
    }
}
