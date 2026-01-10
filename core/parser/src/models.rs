use serde::{Deserialize, Serialize};
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

/// 字幕类型枚举
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
#[serde(rename_all = "UPPERCASE")]
pub enum SubType {
    /// 简体中文
    Chs,
    /// 繁体中文
    Cht,
    /// 日语
    Jpn,
    /// 英语
    Eng,
    /// 未知
    Unknown,
}

impl Default for SubType {
    fn default() -> Self {
        Self::Unknown
    }
}

impl std::fmt::Display for SubType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SubType::Chs => write!(f, "CHS"),
            SubType::Cht => write!(f, "CHT"),
            SubType::Jpn => write!(f, "JPN"),
            SubType::Eng => write!(f, "ENG"),
            SubType::Unknown => write!(f, "UNKNOWN"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ParseResult {
    pub name_en: Option<String>,
    pub name_zh: Option<String>,
    pub name_jp: Option<String>,
    pub episode: Option<i32>,
    pub season: Option<i32>,
    pub subtitle_group: Option<String>,
    pub resolution: Option<String>,
    pub subtitle_language: Vec<SubType>,
}

// 中文数字映射 (支持 1-20)
pub static CHINESE_NUMBER_MAP: phf::Map<&'static str, i32> = phf::phf_map! {
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
    "十一" => 11,
    "十二" => 12,
    "十三" => 13,
    "十四" => 14,
    "十五" => 15,
    "十六" => 16,
    "十七" => 17,
    "十八" => 18,
    "十九" => 19,
    "二十" => 20,
};
