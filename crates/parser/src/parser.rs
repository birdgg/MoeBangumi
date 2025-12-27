use crate::models::{CHINESE_NUMBER_MAP, ParseResult};
use anyhow::{Result, anyhow};
use regex::Regex;
use std::sync::LazyLock;

// 使用 LazyLock 定义全局正则表达式，避免重复编译

// 匹配集数的正则表达式
static EPISODE_PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\d+").unwrap());

// 匹配标题格式的正则表达式，包含标题、集数和其他信息
static TITLE_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?:\[([^\]]+)\])?(?P<season>.*|\[.*])(?P<episode>\(\d{1,3}\)|\[\d+]|\[\d+.?[vV]\d]|第\d+[话話集]|\[第?\d+[话話集]]|\[\d+.?(?:END|完)]|[Ee][Pp]?\d+|\[?特[別别]篇\]?|\[?[總总]集篇\]?|\d{1,4}-\d{1,4}|合集| -? ?\d+(?:[Ee][Nn][Dd]|[vV]\d|完)?(?:[ \]\[-]|$))(?P<others>.*)"
    ).unwrap()
});

// 匹配视频分辨率的正则表达式
static RESOLUTION_1080_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"1080|1920x1080").unwrap());
static RESOLUTION_720_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"720|1280x720").unwrap());
static RESOLUTION_2160_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"2160|4096x2160|4K|4k").unwrap());

// 匹配简体中文字幕的正则表达式
static SUB_CHS_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"[简中]|CHS|SC|GB|GBK|GB2312").unwrap());

// 匹配繁体中文字幕的正则表达式
static SUB_CHT_PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"繁|CHT|BIG5").unwrap());

// 匹配日语字幕的正则表达式
static SUB_JPN_PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[日]|JP|JPSC").unwrap());

// 匹配英语字幕的正则表达式
static SUB_ENG_PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"ENG|英语|英文").unwrap());

// 匹配非法前缀字符的正则表达式
static PREFIX_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"[^\w\s\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff-]").unwrap());

// 匹配季度信息的正则表达式
static SEASON_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"S\d{1,2}|Season \d{1,2}|[第].[季期]").unwrap());

// 匹配方括号的正则表达式
static BRACKET_PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[\[\]]").unwrap());

// 匹配中文字符的正则表达式
static CHS_PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[\u4e00-\u9fff]{2,}").unwrap());

// 匹配日文字符的正则表达式
static JP_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"[\u3040-\u309f\u30a0-\u30ff]{2,}").unwrap());

// 匹配英文字符的正则表达式
static EN_PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[a-zA-Z]{3,}").unwrap());

// 匹配技术规格的正则表达式，需要去掉，免得影响匹配集数
static TECHNICAL_SPECS_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\d+(?:-)?(?:fps|bit|kHz|Hz)").unwrap());

// 匹配所有标点符号、括号、特殊字符
static PUNCTUATION_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"[^\w\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff]").unwrap());

// 匹配连续的空格
static MULTIPLE_SPACES_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\s{2,}").unwrap());

/// 动画文件名解析器
#[derive(Debug, Clone, Default)]
pub struct Parser;

impl Parser {
    /// 创建新的解析器实例
    pub fn new() -> Self {
        Self {}
    }

    /// 从方括号中提取字幕组信息
    fn get_group(name: &str) -> Option<String> {
        BRACKET_PATTERN.split(name).nth(1).map(|s| s.to_string())
    }

    /// 预处理文件名，将中文方括号转换为英文方括号
    fn pre_process(raw_name: &str) -> String {
        let processed = raw_name
            .replace('【', "[")
            .replace('】', "]")
            .replace("～", "~");

        // 移除帧率、比特率等技术规格标记
        TECHNICAL_SPECS_PATTERN
            .replace_all(&processed, "")
            .into_owned()
    }

    /// 处理文件名前缀，移除字幕组信息和无关标记
    fn prefix_process(raw: &str, group: &Option<String>) -> String {
        let mut result = raw.to_string();
        // 移除字幕组信息
        if let Some(group) = group {
            result = result.replace(&format!("[{}]", group), "");
        }

        // 处理前缀
        let raw_process = PREFIX_PATTERN.replace_all(&result, "/").into_owned();
        let arg_group: Vec<&str> = raw_process.split('/').filter(|s| !s.is_empty()).collect();

        let arg_group = if arg_group.len() == 1 {
            arg_group[0].split_whitespace().collect::<Vec<_>>()
        } else {
            arg_group
        };

        // 移除特定标记
        for &arg in &arg_group {
            if (arg.contains("新番") || arg.contains("月番")) || arg.contains("港澳台地区")
            {
                result = result.replace(arg, "");
            }
        }

        result
    }

    /// 处理季度信息，返回处理后的名称、原始季度文本和季度数字
    fn season_process(season_info: &str) -> Option<i32> {
        let name_season = BRACKET_PATTERN.replace_all(season_info, " ").into_owned();
        let seasons: Vec<_> = SEASON_PATTERN
            .find_iter(&name_season)
            .map(|m| m.as_str())
            .collect();

        if seasons.is_empty() {
            return None;
        }

        let season_raw = seasons[0].to_string();

        // 解析季度数字
        let season = if season_raw.contains("Season") || season_raw.contains('S') {
            season_raw
                .replace("Season", "")
                .replace('S', "")
                .trim()
                .parse()
                .unwrap_or(1)
        } else {
            let season_text = season_raw
                .replace(['第', '季', '期'], "")
                .trim()
                .to_string();

            match season_text.parse::<i32>() {
                Ok(num) => num,
                Err(_) => *CHINESE_NUMBER_MAP.get(season_text.as_str()).unwrap_or(&1),
            }
        };

        Some(season)
    }

    /// 处理动画名称，分离出英文、中文和日文标题
    fn name_process(name: &str) -> (Option<String>, Option<String>, Option<String>) {
        let name = BRACKET_PATTERN
            .replace_all(name, " ")
            .into_owned()
            .trim()
            .to_string();
        let name = name
            .replace("(仅限港澳台地区)", "")
            .replace("（仅限港澳台地区）", "");

        // 分割标题
        let splits: Vec<String> = name
            .split(['/', '_'])
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect();

        let mut name_en = None;
        let mut name_zh = None;
        let mut name_jp = None;

        // 识别不同语言的标题
        for item in &splits {
            if JP_PATTERN.is_match(item) && name_jp.is_none() {
                name_jp = Some(Self::clean_name(item));
            } else if CHS_PATTERN.is_match(item) && name_zh.is_none() {
                name_zh = Some(Self::clean_name(item));
            } else if EN_PATTERN.is_match(item) && name_en.is_none() {
                name_en = Some(Self::clean_name(item));
            }
        }

        (name_en, name_zh, name_jp)
    }

    fn clean_name(name: &str) -> String {
        // 使用正则表达式替换所有标点符号、括号等为空格
        let result = PUNCTUATION_PATTERN.replace_all(name, " ").into_owned();

        // 处理连续的空格，替换为单个空格
        let result = MULTIPLE_SPACES_PATTERN
            .replace_all(&result, " ")
            .into_owned();

        // 去除首尾空格
        result.trim().to_string()
    }

    /// 从其他信息中提取字幕类型和分辨率
    fn find_tags(other: &str) -> (Option<String>, Option<String>) {
        let replaced = BRACKET_PATTERN.replace_all(other, " ").into_owned();
        let elements: Vec<&str> = replaced.split_whitespace().collect();

        let mut subs = Vec::new();
        let mut resolution = None;

        for &element in &elements {
            if SUB_CHS_PATTERN.is_match(element) {
                subs.push("CHS");
            }
            if SUB_CHT_PATTERN.is_match(element) {
                subs.push("CHT");
            }
            if SUB_JPN_PATTERN.is_match(element) {
                subs.push("JPN");
            }
            if SUB_ENG_PATTERN.is_match(element) {
                subs.push("ENG");
            }
            // 分辨率检测：优先匹配最高分辨率，已设置则跳过
            if resolution.is_none() {
                if RESOLUTION_2160_PATTERN.is_match(element) {
                    resolution = Some("2160P".to_string());
                } else if RESOLUTION_1080_PATTERN.is_match(element) {
                    resolution = Some("1080P".to_string());
                } else if RESOLUTION_720_PATTERN.is_match(element) {
                    resolution = Some("720P".to_string());
                }
            }
        }

        // 去重
        subs.sort();
        subs.dedup();

        // 连接为字符串，空则返回 None
        let sub_type = if subs.is_empty() {
            None
        } else {
            Some(subs.join("+"))
        };

        (sub_type, resolution)
    }

    #[allow(dead_code)]
    /// 用于移除标题上的 Season 信息，以及其它信息，用于搜索
    pub fn remove_season(name: &str) -> (String, Option<i32>) {
        let mut result = name.to_string();
        let mut season_info = "";
        for ele in SEASON_PATTERN.find_iter(name) {
            season_info = ele.as_str();
            result = result.replace(season_info, "");
        }
        (Self::clean_name(&result), Self::season_process(season_info))
    }

    /// 解析动画文件名，提取所有相关信息
    pub fn parse(&self, file_name: &str) -> Result<ParseResult> {
        let raw_title = file_name.trim().replace('\n', " ");
        let content_title = Self::pre_process(&raw_title);

        // 获取字幕组信息
        let group = Self::get_group(&content_title);

        // 解析标题格式
        let captures = TITLE_PATTERN
            .captures(&content_title)
            .ok_or_else(|| anyhow!("无法解析标题格式"))?;

        let title_info = captures.get(2).map(|m| m.as_str().trim()).unwrap_or("");

        let episode_info = captures.get(3).map(|m| m.as_str().trim()).unwrap_or("");

        let other = captures.get(4).map(|m| m.as_str().trim()).unwrap_or("");

        // 处理前缀
        let process_raw = Self::prefix_process(title_info, &group);

        // 处理季度信息
        let season = Self::season_process(&process_raw);

        // 处理名称
        let (name_en, name_zh, name_jp) = Self::name_process(&process_raw);

        // 处理集数
        let episode = EPISODE_PATTERN
            .find(episode_info)
            .and_then(|m| m.as_str().parse().ok());

        // 处理其他标签
        let (sub_type, resolution) = Self::find_tags(other);

        // 返回解析结果
        Ok(ParseResult {
            name_en,
            name_zh,
            name_jp,
            episode,
            season,
            subtitle_group: group,
            resolution,
            sub_type,
        })
    }
}
