-- | BGM.tv 名称解析模块
--
-- 解析 BGM.tv 风格的番剧名称，提取季度信息并清理标题。
--
-- 使用 megaparsec 实现，支持以下格式：
--
-- - 中文: "第X季", "第X期" (X 可为阿拉伯数字或中文数字 一~十)
-- - 英文: "SX", "Season X", "SEASON2" (大小写不敏感)
--
-- 例如：
--
-- @
-- import Moe.Parsing.Bgmtv (parseBgmtvName)
--
-- let result = parseBgmtvName "我推的孩子 第二季"
-- result.title   -- "我推的孩子"
-- result.season  -- 2
-- @
module Moe.Parsing.Bgmtv
  ( -- * Types
    BgmtvParseResult (..)

    -- * Parser
  , parseBgmtvName

    -- * Internals (exported for testing)
  , chineseSeasonP
  , englishSeasonP
  , seasonMarkerP
  , parseChineseNumber
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isSpace)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Moe.Parsing.Types (chineseNumberMap)
import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | BGM.tv 名称解析结果
--
-- 例如：
--
-- @
-- parseBgmtvName "我推的孩子 第二季"
-- -- BgmtvParseResult { title = "我推的孩子", season = 2 }
--
-- parseBgmtvName "Frieren S02"
-- -- BgmtvParseResult { title = "Frieren", season = 2 }
--
-- parseBgmtvName "魔法使的新娘 第二季 第2部分"
-- -- BgmtvParseResult { title = "魔法使的新娘 第2部分", season = 2 }
-- @
data BgmtvParseResult = BgmtvParseResult
  { title :: Text
  -- ^ 清理后的标题（移除季度信息）
  , season :: Int
  -- ^ 季度号（默认为 1）
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Parser Type
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | 解析 BGM.tv 风格的番剧名称
--
-- 解析规则：
--
-- - "第X季"、"第X期"、"SX"、"Season X" 表示真正的季度，会从名称中移除
-- - "第X部分"、"第Xクール" 表示分割放送，保留在名称中
-- - 无季度信息时默认为第一季
--
-- 例如：
--
-- @
-- parseBgmtvName "魔法使的新娘 第二季 第2部分" -- { title: "魔法使的新娘 第2部分", season: 2 }
-- parseBgmtvName "间谍过家家 第2部分"          -- { title: "间谍过家家 第2部分", season: 1 }
-- parseBgmtvName "Frieren S02"                 -- { title: "Frieren", season: 2 }
-- @
parseBgmtvName :: Text -> BgmtvParseResult
parseBgmtvName input =
  let stripped = T.strip input
   in case runParser bgmtvParser "bgmtv-name" stripped of
        Left _ -> BgmtvParseResult stripped 1
        Right result
          | T.null result.title -> BgmtvParseResult stripped result.season
          | otherwise -> result

--------------------------------------------------------------------------------
-- Main Parser
--------------------------------------------------------------------------------

-- | 主解析器：提取季度标记并构建结果
bgmtvParser :: Parser BgmtvParseResult
bgmtvParser = do
  parts <- many titlePartP
  eof
  pure $ buildResult parts

-- | 标题片段类型
data TitlePart
  = TextPart Text
  | SeasonPart Int
  deriving stock (Show, Eq)

-- | 从片段构建最终结果
buildResult :: [TitlePart] -> BgmtvParseResult
buildResult parts =
  let seasons = [s | SeasonPart s <- parts]
      textParts = [t | TextPart t <- parts]
      cleanedTitle = normalizeSpaces (T.concat textParts)
      seasonNum = if null seasons then 1 else List.maximum seasons
   in BgmtvParseResult
        { title = cleanedTitle
        , season = seasonNum
        }

-- | 清理多余空格
normalizeSpaces :: Text -> Text
normalizeSpaces = T.unwords . T.words

--------------------------------------------------------------------------------
-- Title Part Parser
--------------------------------------------------------------------------------

-- | 解析标题片段（季度标记或普通文本）
titlePartP :: Parser TitlePart
titlePartP = try seasonPartP <|> textPartP

-- | 解析季度标记片段
seasonPartP :: Parser TitlePart
seasonPartP = SeasonPart <$> seasonMarkerP

-- | 解析普通文本片段
textPartP :: Parser TitlePart
textPartP = TextPart <$> normalTextP

-- | 解析普通文本（不匹配季度标记的内容）
normalTextP :: Parser Text
normalTextP = T.singleton <$> anySingle

--------------------------------------------------------------------------------
-- Season Marker Parsers
--------------------------------------------------------------------------------

-- | 季度标记解析器（中文或英文格式）
--
-- 支持：
--
-- - 中文: "第X季", "第X期"
-- - 英文: "SX", "S X", "Season X", "SEASON2"
seasonMarkerP :: Parser Int
seasonMarkerP = try chineseSeasonP <|> try englishSeasonP

-- | 中文季度解析器: 第X季/期
--
-- 支持阿拉伯数字和中文数字（一~十）
chineseSeasonP :: Parser Int
chineseSeasonP = do
  _ <- char '第'
  num <- chineseNumberP <|> L.decimal
  _ <- choice [char '季', char '期']
  pure num

-- | 中文数字解析器 (一~十)
--
-- 使用共享的 chineseNumberMap 避免重复
chineseNumberP :: Parser Int
chineseNumberP = choice $ map makeParser (Map.toList chineseNumberMap)
 where
  makeParser (charText, num) = num <$ string charText

-- | 解析中文数字（用于测试）
--
-- 复用共享的 chineseNumberMap
parseChineseNumber :: Text -> Maybe Int
parseChineseNumber t =
  Map.lookup t chineseNumberMap <|> readMaybe (T.unpack t)

-- | 英文季度解析器: SX, Season X
--
-- 大小写不敏感
englishSeasonP :: Parser Int
englishSeasonP = do
  _ <- char' 's'
  _ <- optional (string' "eason")
  _ <- optional (satisfy isSpace)
  L.decimal
