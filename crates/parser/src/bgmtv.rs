//! BGM.tv 名称解析模块
//!
//! 解析 BGM.tv 风格的番剧名称，提取季度信息并清理标题。

use regex::Regex;
use std::sync::LazyLock;

use crate::models::CHINESE_NUMBER_MAP;

/// 匹配真正的季度信息: "第X季"、"第X期"、"S01"、"Season 2"、"SEASON2" (大小写不敏感)
static SEASON_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?i)(?:第(?P<cn_num>[0-9一二三四五六七八九十]+)(?:季|期))|(?:(?:S(?:EASON)?\s*)(?P<en_num>\d{1,2}))",
    )
    .unwrap()
});

/// 名称解析结果
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameParseResult {
    /// 清理后的名称
    pub title: String,
    /// 季度 (默认为 1)
    pub season: i32,
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
