//! BGM.tv Subject 解析模块
//!
//! 将 BGM.tv 的 Subject/SubjectDetail 解析为标准化的 ParsedSubject 结构。

use bgmtv::models::{Subject, SubjectDetail};
use parser::parse_name;
use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

/// 解析后的 BGM.tv Subject
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct ParsedSubject {
    /// BGM.tv ID
    pub bgmtv_id: i64,
    /// 中文标题 (清理后，不含季度信息)
    pub title_chinese: Option<String>,
    /// 日文标题 (清理后，不含季度信息)
    pub title_japanese: Option<String>,
    /// 季度 (默认为 1)
    pub season: i32,
    /// 放送日期
    pub air_date: Option<String>,
    /// 年份 (从 air_date 解析)
    pub year: Option<i32>,
    /// 平台 (TV, Movie, OVA)
    pub platform: String,
    /// 总集数
    pub total_episodes: i64,
    /// 海报 URL
    pub poster_url: String,
}

/// 从日期字符串解析年份
fn parse_year(date: Option<&str>) -> Option<i32> {
    date.and_then(|d| d.get(0..4)).and_then(|s| s.parse().ok())
}

/// 解析 BGM.tv Subject
pub fn parse_subject(subject: &Subject) -> ParsedSubject {
    let parsed_cn = parse_name(&subject.name_cn);
    let parsed_jp = parse_name(&subject.name);
    let air_date = Some(subject.date.clone()).filter(|s| !s.is_empty());

    ParsedSubject {
        bgmtv_id: subject.id,
        title_chinese: Some(parsed_cn.title).filter(|s| !s.is_empty()),
        title_japanese: Some(parsed_jp.title).filter(|s| !s.is_empty()),
        season: parsed_cn.season,
        year: parse_year(air_date.as_deref()),
        air_date,
        platform: subject.platform.clone(),
        total_episodes: subject.eps,
        poster_url: subject.image.clone(),
    }
}

/// 解析 BGM.tv 详情 SubjectDetail
pub fn parse_subject_detail(detail: &SubjectDetail) -> ParsedSubject {
    let parsed_cn = parse_name(&detail.name_cn);
    let parsed_jp = parse_name(&detail.name);

    ParsedSubject {
        bgmtv_id: detail.id,
        title_chinese: Some(parsed_cn.title).filter(|s| !s.is_empty()),
        title_japanese: Some(parsed_jp.title).filter(|s| !s.is_empty()),
        season: parsed_cn.season,
        year: parse_year(detail.date.as_deref()),
        air_date: detail.date.clone(),
        platform: detail.platform.clone().unwrap_or_default(),
        total_episodes: detail.total_episodes,
        poster_url: detail.images.large.clone(),
    }
}
