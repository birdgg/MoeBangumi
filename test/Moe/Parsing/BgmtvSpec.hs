-- | Tests for BGM.tv name parser
--
-- Test cases based on Rust bgmtv.rs implementation
module Moe.Parsing.BgmtvSpec (tests) where

import Moe.Parsing.Bgmtv
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

--------------------------------------------------------------------------------
-- Test Tree
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Moe.Parsing.Bgmtv"
    [ testGroup "parseBgmtvName" integrationTests
    , testGroup "chineseSeasonP" chineseSeasonTests
    , testGroup "englishSeasonP" englishSeasonTests
    , testGroup "parseChineseNumber" chineseNumberTests
    ]

--------------------------------------------------------------------------------
-- Integration Tests
--------------------------------------------------------------------------------

-- | Integration tests for parseBgmtvName
--
-- Based on Rust bgmtv.rs test cases
integrationTests :: [TestTree]
integrationTests =
  [ testCase "季度 + 分割放送组合: 提取季度，保留分割放送信息" $ do
      let result = parseBgmtvName "魔法使的新娘 第二季 第2部分"
      result.title @?= "魔法使的新娘 第2部分"
      result.season @?= 2
  , testCase "SEASON2 + 分割放送" $ do
      let result = parseBgmtvName "魔法使いの嫁 SEASON2 第2クール"
      result.title @?= "魔法使いの嫁 第2クール"
      result.season @?= 2
  , testCase "纯分割放送: 默认第一季，保留分割放送信息" $ do
      let result = parseBgmtvName "间谍过家家 第2部分"
      result.title @?= "间谍过家家 第2部分"
      result.season @?= 1
  , testCase "日语 クール 格式" $ do
      let result = parseBgmtvName "SPY×FAMILY 第2クール"
      result.title @?= "SPY×FAMILY 第2クール"
      result.season @?= 1
  , testCase "纯季度信息 - 中文第X季" $ do
      let result = parseBgmtvName "我推的孩子 第二季"
      result.title @?= "我推的孩子"
      result.season @?= 2
  , testCase "纯季度信息 - 中文第X期" $ do
      let result = parseBgmtvName "葬送的芙莉莲 第1期"
      result.title @?= "葬送的芙莉莲"
      result.season @?= 1
  , testCase "无季度信息时默认为第一季" $ do
      let result = parseBgmtvName "无职转生"
      result.title @?= "无职转生"
      result.season @?= 1
  , testCase "英文格式 S02" $ do
      let result = parseBgmtvName "Frieren S02"
      result.title @?= "Frieren"
      result.season @?= 2
  , testCase "英文格式 s02 (小写)" $ do
      let result = parseBgmtvName "Frieren s02"
      result.title @?= "Frieren"
      result.season @?= 2
  , testCase "英文格式 Season 2 (混合大小写带空格)" $ do
      let result = parseBgmtvName "Spy x Family Season 2"
      result.title @?= "Spy x Family"
      result.season @?= 2
  , testCase "英文格式 season3 (全小写无空格)" $ do
      let result = parseBgmtvName "Attack on Titan season3"
      result.title @?= "Attack on Titan"
      result.season @?= 3
  , testCase "英文格式 SEASON3 (全大写无空格)" $ do
      let result = parseBgmtvName "Attack on Titan SEASON3"
      result.title @?= "Attack on Titan"
      result.season @?= 3
  ]

--------------------------------------------------------------------------------
-- Unit Tests: Chinese Season Parser
--------------------------------------------------------------------------------

chineseSeasonTests :: [TestTree]
chineseSeasonTests =
  [ testCase "第一季 -> 1" $ do
      let result = runParser chineseSeasonP "" "第一季"
      result @?= Right 1
  , testCase "第二季 -> 2" $ do
      let result = runParser chineseSeasonP "" "第二季"
      result @?= Right 2
  , testCase "第三期 -> 3" $ do
      let result = runParser chineseSeasonP "" "第三期"
      result @?= Right 3
  , testCase "第10季 -> 10" $ do
      let result = runParser chineseSeasonP "" "第10季"
      result @?= Right 10
  , testCase "第十季 -> 10" $ do
      let result = runParser chineseSeasonP "" "第十季"
      result @?= Right 10
  , testCase "第2部分 should fail (not 季/期)" $ do
      let result = runParser chineseSeasonP "" "第2部分"
      assertBool "should fail" (isLeft result)
  , testCase "第2クール should fail (not 季/期)" $ do
      let result = runParser chineseSeasonP "" "第2クール"
      assertBool "should fail" (isLeft result)
  ]

--------------------------------------------------------------------------------
-- Unit Tests: English Season Parser
--------------------------------------------------------------------------------

englishSeasonTests :: [TestTree]
englishSeasonTests =
  [ testCase "S1 -> 1" $ do
      let result = runParser englishSeasonP "" "S1"
      result @?= Right 1
  , testCase "S02 -> 2" $ do
      let result = runParser englishSeasonP "" "S02"
      result @?= Right 2
  , testCase "s3 -> 3 (lowercase)" $ do
      let result = runParser englishSeasonP "" "s3"
      result @?= Right 3
  , testCase "Season 2 -> 2" $ do
      let result = runParser englishSeasonP "" "Season 2"
      result @?= Right 2
  , testCase "SEASON3 -> 3 (uppercase)" $ do
      let result = runParser englishSeasonP "" "SEASON3"
      result @?= Right 3
  , testCase "season4 -> 4 (lowercase no space)" $ do
      let result = runParser englishSeasonP "" "season4"
      result @?= Right 4
  ]

--------------------------------------------------------------------------------
-- Unit Tests: Chinese Number Parser
--------------------------------------------------------------------------------

chineseNumberTests :: [TestTree]
chineseNumberTests =
  [ testCase "一 -> 1" $ parseChineseNumber "一" @?= Just 1
  , testCase "二 -> 2" $ parseChineseNumber "二" @?= Just 2
  , testCase "三 -> 3" $ parseChineseNumber "三" @?= Just 3
  , testCase "四 -> 4" $ parseChineseNumber "四" @?= Just 4
  , testCase "五 -> 5" $ parseChineseNumber "五" @?= Just 5
  , testCase "六 -> 6" $ parseChineseNumber "六" @?= Just 6
  , testCase "七 -> 7" $ parseChineseNumber "七" @?= Just 7
  , testCase "八 -> 8" $ parseChineseNumber "八" @?= Just 8
  , testCase "九 -> 9" $ parseChineseNumber "九" @?= Just 9
  , testCase "十 -> 10" $ parseChineseNumber "十" @?= Just 10
  , testCase "1 -> 1 (arabic)" $ parseChineseNumber "1" @?= Just 1
  , testCase "10 -> 10 (arabic)" $ parseChineseNumber "10" @?= Just 10
  , testCase "invalid -> Nothing" $ parseChineseNumber "abc" @?= Nothing
  ]
