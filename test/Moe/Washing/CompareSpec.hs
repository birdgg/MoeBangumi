-- | Tests for washing (torrent upgrade) comparison
module Moe.Washing.CompareSpec (tests) where

import Moe.Setting (PrioritySettings (..))
import Moe.Subtitle (Subtitle (..), SubtitlePattern (..))
import Moe.Washing.Compare
import Moe.Washing.Parser (parseTitle)
import Moe.Washing.Types
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (compare, group)

--------------------------------------------------------------------------------
-- Test Cases
--------------------------------------------------------------------------------

-- | Test data: Episode 2 torrent titles from different groups
-- Expected winner: LoliHouse (first in priority list)
-- Using NonEmpty for safe head access
episode2Titles :: NonEmpty Text
episode2Titles =
  "[LoliHouse] 安闲领主的愉快领地防卫 / Okiraku Ryoushu no Tanoshii Ryouchi Bouei - 02 [WebRip 1080p HEVC-10bit AAC][简繁内封字幕]"
    :| [ "[Yuuki] 安逸领主的愉快领地防卫 / Okiraku Ryoushu - 02 HEVC 1080P 简繁日三语 MKV"
       , "[黒ネズミたち] 安逸领主的愉快领地防卫～用生产系魔术将无名村改造成最强要塞都市～ / Okiraku Ryoushu - 02 (CR 1920x1080 AVC AAC MKV)"
       , "[黒ネズミたち] 安逸领主的愉快领地防卫～用生产系魔术将无名村改造成最强要塞都市～ / Okiraku Ryoushu - 02 (Baha 1920x1080 AVC AAC MP4)"
       , "[ANi] 安逸领主的愉快领地防卫～用生产系魔术将无名村改造成最强要塞都市～ - 02 [1080P][Baha][WEB-DL][AAC AVC][CHT][MP4]"
       ]

-- | Convert NonEmpty to list for indexing
titleList :: [Text]
titleList = toList episode2Titles

-- | Safe index access helper
unsafeIdx :: [a] -> Int -> a
unsafeIdx xs i = fromMaybe (error $ "Index out of bounds: " <> show i) (xs !!? i)

-- | Priority settings for test:
-- - Group priority: lolihouse first, then ANi
-- - Language priority: CHS (simplified Chinese) first
testPrioritySettings :: PrioritySettings
testPrioritySettings =
  PrioritySettings
    { groups = ["lolihouse", "ANi"]
    , languages = [SubtitlePattern [CHS]]
    }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- | All washing comparison tests
tests :: TestTree
tests =
  testGroup
    "Moe.Washing.Compare"
    [ testGroup "Parser" parserTests
    , testGroup "Compare with settings" compareWithSettingsTests
    , testGroup "Episode 2 washing scenario" episode2WashingTests
    ]

-- | Parser tests
parserTests :: [TestTree]
parserTests =
  [ testCase "Parse LoliHouse title" $ do
      let title = head episode2Titles
          quality = parseTitle title
      quality.group @?= Just "LoliHouse"
      quality.resolution @?= Just R1080p
      quality.videoCodec @?= Just H265
      quality.audioCodec @?= Just AAC
      quality.isBDRip @?= False

  , testCase "Parse Yuuki title" $ do
      let title = unsafeIdx titleList 1
          quality = parseTitle title
      quality.group @?= Just "Yuuki"
      quality.resolution @?= Just R1080p
      quality.videoCodec @?= Just H265
      quality.isBDRip @?= False

  , testCase "Parse ANi title" $ do
      let title = unsafeIdx titleList 4
          quality = parseTitle title
      quality.group @?= Just "ANi"
      quality.resolution @?= Just R1080p
      quality.videoCodec @?= Just H264
      quality.audioCodec @?= Just AAC
      quality.subtitles @?= [CHT]
      quality.isBDRip @?= False
  ]

-- | Compare with settings tests
compareWithSettingsTests :: [TestTree]
compareWithSettingsTests =
  [ testCase "Group in priority list beats group not in list" $ do
      let lolihouse = parseTitle (head episode2Titles)
          yuuki = parseTitle (unsafeIdx titleList 1)
      -- LoliHouse is in priority list, Yuuki is not
      compareWithSettings testPrioritySettings yuuki lolihouse @?= Better

  , testCase "Higher priority group beats lower priority group" $ do
      let lolihouse = parseTitle (head episode2Titles)
          ani = parseTitle (unsafeIdx titleList 4)
      -- Both in list: lolihouse (idx 0) > ANi (idx 1), so ANi is Worse
      compareWithSettings testPrioritySettings lolihouse ani @?= Worse

  , testCase "Two groups not in list compare by score" $ do
      let yuuki = parseTitle (unsafeIdx titleList 1)       -- HEVC
          kuroNezumi = parseTitle (unsafeIdx titleList 2)  -- AVC
      -- Both not in priority list (Yuuki, 黒ネズミたち), Yuuki has HEVC (better codec)
      compareWithSettings testPrioritySettings kuroNezumi yuuki @?= Better
  ]

-- | Episode 2 washing scenario test
-- Given the 5 titles, find the best one using washing logic
episode2WashingTests :: [TestTree]
episode2WashingTests =
  [ testCase "LoliHouse should be selected as best (group priority)" $ do
      let qualities = map parseTitle titleList
          lolihouse = parseTitle (head episode2Titles)
          -- Compare LoliHouse against all others
          -- compare current new -> if new is better, return Better
          -- So compare lolihouse (as current) vs others -> should all be Worse
          results = map (compareWithSettings testPrioritySettings lolihouse) (drop 1 qualities)
      all (== Worse) results @?= True

  , testCase "LoliHouse wins against Yuuki" $ do
      let lolihouse = parseTitle (head episode2Titles)
          yuuki = parseTitle (unsafeIdx titleList 1)
      -- Comparing: current=lolihouse, new=yuuki -> yuuki should be Worse
      compareWithSettings testPrioritySettings lolihouse yuuki @?= Worse

  , testCase "LoliHouse wins against 黒ネズミたち (CR)" $ do
      let lolihouse = parseTitle (head episode2Titles)
          kuroNezumi = parseTitle (unsafeIdx titleList 2)
      compareWithSettings testPrioritySettings lolihouse kuroNezumi @?= Worse

  , testCase "LoliHouse wins against 黒ネズミたち (Baha)" $ do
      let lolihouse = parseTitle (head episode2Titles)
          kuroNezumiBaha = parseTitle (unsafeIdx titleList 3)
      compareWithSettings testPrioritySettings lolihouse kuroNezumiBaha @?= Worse

  , testCase "LoliHouse wins against ANi" $ do
      let lolihouse = parseTitle (head episode2Titles)
          ani = parseTitle (unsafeIdx titleList 4)
      compareWithSettings testPrioritySettings lolihouse ani @?= Worse

  , testCase "Select best from all candidates" $ do
      -- Simulate the washing selection process:
      -- Start with first candidate, compare each subsequent one
      let qualities = map parseTitle titleList
          selectBest :: [TorrentQuality] -> TorrentQuality
          selectBest [] = error "Empty list"
          selectBest [x] = x
          selectBest (current:new:rest) =
            case compareWithSettings testPrioritySettings current new of
              Better -> selectBest (new:rest)  -- new is better, use it
              _      -> selectBest (current:rest)  -- keep current
          best = selectBest qualities
      -- The best should be LoliHouse
      best.group @?= Just "LoliHouse"
  ]
