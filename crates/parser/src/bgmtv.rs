use crate::models::CHINESE_NUMBER_MAP;
use regex::Regex;
use std::sync::LazyLock;

/// BGM.tv 名称解析结果
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BgmtvParseResult {
    /// 番剧名称
    pub name: String,
    /// 季度/部分
    pub season: Option<i32>,
}

// 匹配真正的季度信息: "第X季"、"第X期"、"S01"、"Season 2"、"SEASON2" (大小写不敏感)
static SEASON_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?i)(?:第(?P<cn_num>[0-9一二三四五六七八九十]+)(?:季|期))|(?:(?:S(?:EASON)?\s*)(?P<en_num>\d{1,2}))",
    )
    .unwrap()
});

// 匹配分割放送信息: "第X部分"、"第Xクール" (这些不是季度，只是分割放送)
static SPLIT_BROADCAST_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"第[0-9一二三四五六七八九十]+(?:部分|クール)").unwrap()
});

/// 解析 BGM.tv 风格的番剧名称
///
/// 解析规则：
/// - "第X季"、"第X期"、"SX"、"Season X" 表示真正的季度
/// - "第X部分"、"第Xクール" 表示分割放送，会从名称中移除但不影响季度
/// - 无季度信息时默认为第一季
///
/// 例如：
/// - "魔法使的新娘 第二季 第2部分" -> { name: "魔法使的新娘", season: 2 }
/// - "魔法使いの嫁 SEASON2 第2クール" -> { name: "魔法使いの嫁", season: 2 }
/// - "间谍过家家 第2部分" -> { name: "间谍过家家", season: 1 }
/// - "我推的孩子 第二季" -> { name: "我推的孩子", season: 2 }
/// - "Frieren S02" -> { name: "Frieren", season: 2 }
/// - "无职转生" -> { name: "无职转生", season: 1 }
pub fn parse_bgmtv_name(name: &str) -> BgmtvParseResult {
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

    // Step 2: 移除分割放送信息 (第X部分、第Xクール)
    result_name = SPLIT_BROADCAST_PATTERN
        .replace(&result_name, "")
        .to_string();

    // Step 3: 清理多余空格
    result_name = result_name.split_whitespace().collect::<Vec<_>>().join(" ");

    BgmtvParseResult {
        name: result_name,
        season: Some(season),
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
    fn test_parse_bgmtv_name() {
        // 季度 + 分割放送组合: 提取季度，移除分割放送信息
        let result = parse_bgmtv_name("魔法使的新娘 第二季 第2部分");
        assert_eq!(result.name, "魔法使的新娘");
        assert_eq!(result.season, Some(2));

        let result = parse_bgmtv_name("魔法使いの嫁 SEASON2 第2クール");
        assert_eq!(result.name, "魔法使いの嫁");
        assert_eq!(result.season, Some(2));

        // 纯分割放送: 默认第一季，移除分割放送信息
        let result = parse_bgmtv_name("间谍过家家 第2部分");
        assert_eq!(result.name, "间谍过家家");
        assert_eq!(result.season, Some(1));

        let result = parse_bgmtv_name("SPY×FAMILY 第2クール");
        assert_eq!(result.name, "SPY×FAMILY");
        assert_eq!(result.season, Some(1));

        // 纯季度信息
        let result = parse_bgmtv_name("我推的孩子 第二季");
        assert_eq!(result.name, "我推的孩子");
        assert_eq!(result.season, Some(2));

        let result = parse_bgmtv_name("葬送的芙莉莲 第1期");
        assert_eq!(result.name, "葬送的芙莉莲");
        assert_eq!(result.season, Some(1));

        // 无季度信息时默认为第一季
        let result = parse_bgmtv_name("无职转生");
        assert_eq!(result.name, "无职转生");
        assert_eq!(result.season, Some(1));

        // 英文格式测试 S02
        let result = parse_bgmtv_name("Frieren S02");
        assert_eq!(result.name, "Frieren");
        assert_eq!(result.season, Some(2));

        // 英文格式测试 s02 (小写)
        let result = parse_bgmtv_name("Frieren s02");
        assert_eq!(result.name, "Frieren");
        assert_eq!(result.season, Some(2));

        // 英文格式测试 Season 2 (混合大小写带空格)
        let result = parse_bgmtv_name("Spy x Family Season 2");
        assert_eq!(result.name, "Spy x Family");
        assert_eq!(result.season, Some(2));

        // 英文格式测试 season2 (全小写无空格)
        let result = parse_bgmtv_name("Attack on Titan season3");
        assert_eq!(result.name, "Attack on Titan");
        assert_eq!(result.season, Some(3));

        // 英文格式测试 SEASON2 (全大写无空格)
        let result = parse_bgmtv_name("Attack on Titan SEASON3");
        assert_eq!(result.name, "Attack on Titan");
        assert_eq!(result.season, Some(3));
    }
}
