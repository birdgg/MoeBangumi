use quick_xml::events::Event;
use quick_xml::reader::Reader;

use crate::models::RssItem;
use crate::RssError;

/// Parse a Mikan RSS feed from raw XML bytes
pub fn parse_mikan_feed(xml: &[u8]) -> Result<Vec<RssItem>, RssError> {
    let mut reader = Reader::from_reader(xml);
    reader.config_mut().trim_text(true);

    let mut items = Vec::new();
    let mut buf = Vec::new();

    let mut current_item: Option<RssItemBuilder> = None;
    let mut current_element = String::new();

    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(e)) => {
                let name = String::from_utf8_lossy(e.name().as_ref()).to_string();
                current_element = name.clone();

                if name == "item" {
                    current_item = Some(RssItemBuilder::default());
                }

                // Handle <enclosure> attributes
                if name == "enclosure" {
                    if let Some(ref mut item) = current_item {
                        for attr in e.attributes().flatten() {
                            let key = String::from_utf8_lossy(attr.key.as_ref());
                            let value = String::from_utf8_lossy(&attr.value);
                            if key.as_ref() == "url" {
                                item.torrent_url = Some(value.to_string());
                                item.info_hash = extract_info_hash_from_url(&value);
                            }
                        }
                    }
                }
            }
            Ok(Event::Empty(e)) => {
                let name = String::from_utf8_lossy(e.name().as_ref()).to_string();

                // Handle self-closing <enclosure ... />
                if name == "enclosure" {
                    if let Some(ref mut item) = current_item {
                        for attr in e.attributes().flatten() {
                            let key = String::from_utf8_lossy(attr.key.as_ref());
                            let value = String::from_utf8_lossy(&attr.value);
                            if key.as_ref() == "url" {
                                item.torrent_url = Some(value.to_string());
                                item.info_hash = extract_info_hash_from_url(&value);
                            }
                        }
                    }
                }
            }
            Ok(Event::End(e)) => {
                let name = String::from_utf8_lossy(e.name().as_ref()).to_string();

                if name == "item" {
                    if let Some(builder) = current_item.take() {
                        if let Some(item) = builder.build() {
                            items.push(item);
                        }
                    }
                }
                current_element.clear();
            }
            Ok(Event::Text(e)) => {
                if let Some(ref mut item) = current_item {
                    let text = e.unescape().unwrap_or_default().to_string();
                    if !text.is_empty() && current_element == "title" {
                        item.title = Some(text);
                    }
                }
            }
            Ok(Event::Eof) => break,
            Err(e) => return Err(RssError::Parse(format!("XML parse error: {}", e))),
            _ => {}
        }
        buf.clear();
    }

    Ok(items)
}

#[derive(Default)]
struct RssItemBuilder {
    title: Option<String>,
    torrent_url: Option<String>,
    info_hash: Option<String>,
}

impl RssItemBuilder {
    fn build(self) -> Option<RssItem> {
        Some(RssItem {
            title: self.title?,
            torrent_url: self.torrent_url?,
            info_hash: self.info_hash?,
        })
    }
}

/// Extract info hash from Mikan torrent URL
/// Example: https://mikanani.me/Download/20251220/37246624cfd73a8e8caf1bb54fea681a7b823a47.torrent
fn extract_info_hash_from_url(url: &str) -> Option<String> {
    let url_lower = url.to_lowercase();
    for segment in url_lower.split('/') {
        let segment = segment.trim_end_matches(".torrent");
        if segment.len() == 40 && segment.chars().all(|c| c.is_ascii_hexdigit()) {
            return Some(segment.to_string());
        }
    }
    None
}
