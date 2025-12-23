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

// 匹配 "第X部分"、"第X季"、"第X期"、"第Xクール" 等模式
static BGMTV_SEASON_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"第([0-9一二三四五六七八九十]+)(部分|季|期|クール)").unwrap());

/// 解析 BGM.tv 风格的番剧名称
///
/// 注意："部分"、"クール" 表示分割放送，始终解析为第一季
///
/// 例如：
/// - "间谍过家家 第2部分" -> BgmtvParseResult { name: "间谍过家家", season: Some(1) }
/// - "SPY×FAMILY 第2クール" -> BgmtvParseResult { name: "SPY×FAMILY", season: Some(1) }
/// - "我推的孩子 第二季" -> BgmtvParseResult { name: "我推的孩子", season: Some(2) }
pub fn parse_bgmtv_name(name: &str) -> BgmtvParseResult {
    let name = name.trim();

    if let Some(captures) = BGMTV_SEASON_PATTERN.captures(name) {
        let season_str = captures.get(1).map(|m| m.as_str()).unwrap_or("");
        let suffix = captures.get(2).map(|m| m.as_str()).unwrap_or("");

        // "部分"、"クール" 表示分割放送，始终为第一季
        let season = if suffix == "部分" || suffix == "クール" {
            1
        } else {
            parse_season_number(season_str)
        };

        // 移除季度信息，获取纯净的番剧名称
        let bangumi_name = BGMTV_SEASON_PATTERN
            .replace(name, "")
            .trim()
            .to_string();

        BgmtvParseResult {
            name: bangumi_name,
            season: Some(season),
        }
    } else {
        BgmtvParseResult {
            name: name.to_string(),
            season: None,
        }
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
        // "部分" 表示分割放送，始终为第一季
        let result = parse_bgmtv_name("间谍过家家 第2部分");
        assert_eq!(result.name, "间谍过家家");
        assert_eq!(result.season, Some(1));

        // "クール" 表示分割放送，始终为第一季
        let result = parse_bgmtv_name("SPY×FAMILY 第2クール");
        assert_eq!(result.name, "SPY×FAMILY");
        assert_eq!(result.season, Some(1));

        let result = parse_bgmtv_name("我推的孩子 第二季");
        assert_eq!(result.name, "我推的孩子");
        assert_eq!(result.season, Some(2));

        let result = parse_bgmtv_name("葬送的芙莉莲 第1期");
        assert_eq!(result.name, "葬送的芙莉莲");
        assert_eq!(result.season, Some(1));

        let result = parse_bgmtv_name("无职转生");
        assert_eq!(result.name, "无职转生");
        assert_eq!(result.season, None);
    }
}
