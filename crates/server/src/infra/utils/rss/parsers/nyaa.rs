use chrono::DateTime;
use quick_xml::events::Event;
use quick_xml::reader::Reader;

use super::super::models::RssItem;
use super::super::RssError;

/// Parse a Nyaa RSS feed from raw XML bytes
pub fn parse_nyaa_feed(xml: &[u8]) -> Result<Vec<RssItem>, RssError> {
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
                    if !text.is_empty() {
                        match current_element.as_str() {
                            "title" => item.title = Some(text),
                            "link" => item.torrent_url = Some(text),
                            "nyaa:infoHash" => item.info_hash = Some(text.to_lowercase()),
                            "pubDate" => item.pub_date = parse_rfc2822_to_iso8601(&text),
                            _ => {}
                        }
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
    pub_date: Option<String>,
}

impl RssItemBuilder {
    fn build(self) -> Option<RssItem> {
        Some(RssItem {
            title: self.title?,
            torrent_url: self.torrent_url?,
            info_hash: self.info_hash?,
            pub_date: self.pub_date,
        })
    }
}

/// Parse RFC 2822 date to ISO 8601 format for easy string comparison
fn parse_rfc2822_to_iso8601(date_str: &str) -> Option<String> {
    DateTime::parse_from_rfc2822(date_str)
        .ok()
        .map(|dt| dt.to_rfc3339())
}
