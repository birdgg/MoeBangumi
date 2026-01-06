use regex::Regex;
use serde::{Deserialize, Serialize};
use std::sync::LazyLock;
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

use crate::models::{Subject, SubjectDetail};

/// 中文数字映射
static CHINESE_NUMBER_MAP: phf::Map<&'static str, i32> = phf::phf_map! {
    "一" => 1,
    "二" => 2,
    "三" => 3,
    "四" => 4,
    "五" => 5,
    "六" => 6,
    "七" => 7,
    "八" => 8,
    "九" => 9,
    "十" => 10,
};

// 匹配真正的季度信息: "第X季"、"第X期"、"S01"、"Season 2"、"SEASON2" (大小写不敏感)
static SEASON_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?i)(?:第(?P<cn_num>[0-9一二三四五六七八九十]+)(?:季|期))|(?:(?:S(?:EASON)?\s*)(?P<en_num>\d{1,2}))",
    )
    .unwrap()
});


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

/// 名称解析结果 (内部使用)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameParseResult {
    /// 清理后的名称
    pub title: String,
    /// 季度 (默认为 1)
    pub season: i32,
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

/// 解析 BGM.tv 风格的番剧名称
///
/// 解析规则：
/// - "第X季"、"第X期"、"SX"、"Season X" 表示真正的季度，会从名称中移除
/// - "第X部分"、"第Xクール" 表示分割放送，保留在名称中
/// - 无季度信息时默认为第一季
///
/// 例如：
/// - "魔法使的新娘 第二季 第2部分" -> { title: "魔法使的新娘 第2部分", season: 2 }
/// - "魔法使いの嫁 SEASON2 第2クール" -> { title: "魔法使いの嫁 第2クール", season: 2 }
/// - "间谍过家家 第2部分" -> { title: "间谍过家家 第2部分", season: 1 }
/// - "我推的孩子 第二季" -> { title: "我推的孩子", season: 2 }
/// - "Frieren S02" -> { title: "Frieren", season: 2 }
/// - "无职转生" -> { title: "无职转生", season: 1 }
pub fn parse_name(name: &str) -> NameParseResult {
    let name = name.trim();
    let mut season = 1;
    let mut result_name = name.to_string();

    // Step 1: 提取真正的季度信息
    if let Some(captures) = SEASON_PATTERN.captures(name) {
        if let Some(cn_num) = captures.name("cn_num") {
            season = parse_season_number(cn_num.as_str());
        } else if let Some(en_num) = captures.name("en_num") {
            season = en_num.as_str().parse().unwrap_or(1);
        }
        // 从名称中移除季度信息
        result_name = SEASON_PATTERN.replace(&result_name, "").to_string();
    }

    // Step 2: 清理多余空格
    result_name = result_name.split_whitespace().collect::<Vec<_>>().join(" ");

    NameParseResult {
        title: result_name,
        season,
    }
}

/// 解析季度数字（支持阿拉伯数字和中文数字）
fn parse_season_number(s: &str) -> i32 {
    // 先尝试解析阿拉伯数字
    if let Ok(num) = s.parse::<i32>() {
        return num;
    }

    // 尝试中文数字
    if let Some(&num) = CHINESE_NUMBER_MAP.get(s) {
        return num;
    }

    // 默认返回 1
    1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_name() {
        // 季度 + 分割放送组合: 提取季度，保留分割放送信息
        let result = parse_name("魔法使的新娘 第二季 第2部分");
        assert_eq!(result.title, "魔法使的新娘 第2部分");
        assert_eq!(result.season, 2);

        let result = parse_name("魔法使いの嫁 SEASON2 第2クール");
        assert_eq!(result.title, "魔法使いの嫁 第2クール");
        assert_eq!(result.season, 2);

        // 纯分割放送: 默认第一季，保留分割放送信息
        let result = parse_name("间谍过家家 第2部分");
        assert_eq!(result.title, "间谍过家家 第2部分");
        assert_eq!(result.season, 1);

        let result = parse_name("SPY×FAMILY 第2クール");
        assert_eq!(result.title, "SPY×FAMILY 第2クール");
        assert_eq!(result.season, 1);

        // 纯季度信息
        let result = parse_name("我推的孩子 第二季");
        assert_eq!(result.title, "我推的孩子");
        assert_eq!(result.season, 2);

        let result = parse_name("葬送的芙莉莲 第1期");
        assert_eq!(result.title, "葬送的芙莉莲");
        assert_eq!(result.season, 1);

        // 无季度信息时默认为第一季
        let result = parse_name("无职转生");
        assert_eq!(result.title, "无职转生");
        assert_eq!(result.season, 1);

        // 英文格式测试 S02
        let result = parse_name("Frieren S02");
        assert_eq!(result.title, "Frieren");
        assert_eq!(result.season, 2);

        // 英文格式测试 s02 (小写)
        let result = parse_name("Frieren s02");
        assert_eq!(result.title, "Frieren");
        assert_eq!(result.season, 2);

        // 英文格式测试 Season 2 (混合大小写带空格)
        let result = parse_name("Spy x Family Season 2");
        assert_eq!(result.title, "Spy x Family");
        assert_eq!(result.season, 2);

        // 英文格式测试 season3 (全小写无空格)
        let result = parse_name("Attack on Titan season3");
        assert_eq!(result.title, "Attack on Titan");
        assert_eq!(result.season, 3);

        // 英文格式测试 SEASON2 (全大写无空格)
        let result = parse_name("Attack on Titan SEASON3");
        assert_eq!(result.title, "Attack on Titan");
        assert_eq!(result.season, 3);
    }
}
