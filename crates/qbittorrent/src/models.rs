use serde::Serialize;

/// Request to add torrents via URLs
#[derive(Debug, Clone, Default, Serialize)]
pub struct AddTorrentRequest {
    /// URLs separated by newlines (HTTP, HTTPS, magnet links)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub urls: Option<String>,
    /// Download folder
    #[serde(skip_serializing_if = "Option::is_none")]
    pub savepath: Option<String>,
    /// Category for the torrent
    #[serde(skip_serializing_if = "Option::is_none")]
    pub category: Option<String>,
    /// Tags for the torrent (comma-separated)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<String>,
}

impl AddTorrentRequest {
    /// Create a new request with a single URL
    pub fn with_url(url: impl Into<String>) -> Self {
        Self {
            urls: Some(url.into()),
            ..Default::default()
        }
    }

    /// Create a new request with multiple URLs
    pub fn with_urls(urls: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let urls_str = urls.into_iter().map(Into::into).collect::<Vec<_>>().join("\n");
        Self {
            urls: Some(urls_str),
            ..Default::default()
        }
    }

    /// Set the save path
    pub fn savepath(mut self, path: impl Into<String>) -> Self {
        self.savepath = Some(path.into());
        self
    }

    /// Set the category
    pub fn category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    /// Set tags (will be joined with comma)
    pub fn tags(mut self, tags: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let tags_str = tags.into_iter().map(Into::into).collect::<Vec<_>>().join(",");
        self.tags = Some(tags_str);
        self
    }

    /// Add a single tag
    pub fn add_tag(mut self, tag: impl Into<String>) -> Self {
        let tag = tag.into();
        self.tags = Some(match self.tags {
            Some(existing) => format!("{},{}", existing, tag),
            None => tag,
        });
        self
    }
}
