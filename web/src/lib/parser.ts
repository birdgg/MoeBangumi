export interface BgmtvParseResult {
  name: string
  season: number | null
}

const CHINESE_NUMBER_MAP: Record<string, number> = {
  '一': 1,
  '二': 2,
  '三': 3,
  '四': 4,
  '五': 5,
  '六': 6,
  '七': 7,
  '八': 8,
  '九': 9,
  '十': 10,
}

// 匹配 "第X部分"、"第X季"、"第X期"、"第Xクール" 等模式
const BGMTV_SEASON_PATTERN = /第([0-9一二三四五六七八九十]+)(部分|季|期|クール)/

/**
 * 解析季度数字（支持阿拉伯数字和中文数字）
 */
function parseSeasonNumber(s: string): number {
  const num = parseInt(s, 10)
  if (!isNaN(num)) {
    return num
  }
  return CHINESE_NUMBER_MAP[s] ?? 1
}

/**
 * 解析 BGM.tv 风格的番剧名称
 *
 * 注意："部分"、"クール" 表示分割放送，始终解析为第一季
 *
 * @example
 * parseBgmtvName("间谍过家家 第2部分") // { name: "间谍过家家", season: 1 }
 * parseBgmtvName("SPY×FAMILY 第2クール") // { name: "SPY×FAMILY", season: 1 }
 * parseBgmtvName("我推的孩子 第二季") // { name: "我推的孩子", season: 2 }
 */
export function parseBgmtvName(name: string): BgmtvParseResult {
  const trimmed = name.trim()
  const match = BGMTV_SEASON_PATTERN.exec(trimmed)

  if (match) {
    const seasonStr = match[1]
    const suffix = match[2]

    // "部分"、"クール" 表示分割放送，始终为第一季
    const season = suffix === '部分' || suffix === 'クール' ? 1 : parseSeasonNumber(seasonStr)

    // 移除季度信息，获取纯净的番剧名称
    const bangumiName = trimmed.replace(BGMTV_SEASON_PATTERN, '').trim()

    return {
      name: bangumiName,
      season,
    }
  }

  return {
    name: trimmed,
    season: null,
  }
}
