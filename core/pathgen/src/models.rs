/// Information required for generating episode download paths
#[derive(Debug, Clone)]
pub struct PathInfo {
    /// Chinese title (primary display name)
    pub title: String,
    /// Year of release
    pub year: i32,
    /// Season number
    pub season: i32,
    /// Episode number
    pub episode: i32,
    /// TMDB ID for external identification
    pub tmdb_id: Option<i64>,
    /// Content type (TV, Movie, OVA, etc.)
    pub kind: Option<String>,
}

impl PathInfo {
    /// Create a new PathInfo with required fields
    pub fn new(title: impl Into<String>, year: i32, season: i32, episode: i32) -> Self {
        Self {
            title: title.into(),
            year,
            season,
            episode,
            tmdb_id: None,
            kind: Some("tv".to_string()),
        }
    }

    /// Set the TMDB ID
    pub fn with_tmdb_id(mut self, tmdb_id: i64) -> Self {
        self.tmdb_id = Some(tmdb_id);
        self
    }

    /// Set the content kind
    pub fn with_kind(mut self, kind: impl Into<String>) -> Self {
        self.kind = Some(kind.into());
        self
    }

    /// Check if this is a movie
    pub fn is_movie(&self) -> bool {
        self.kind.as_deref() == Some("movie")
    }

    /// Check if this is a special (SP, OVA, etc.)
    ///
    /// Specials are identified by:
    /// - season == 0 (Plex convention)
    /// - kind == "special"
    pub fn is_special(&self) -> bool {
        self.season == 0 || self.kind.as_deref() == Some("special")
    }

    /// Create a PathInfo for special content
    pub fn new_special(title: impl Into<String>, year: i32, special_number: i32) -> Self {
        Self {
            title: title.into(),
            year,
            season: 0,
            episode: special_number,
            tmdb_id: None,
            kind: Some("special".to_string()),
        }
    }
}
