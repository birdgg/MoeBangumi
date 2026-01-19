-- | Tests for anime filename parser
module Moe.Parsing.AnimeSpec (tests) where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Moe.Subtitle (Subtitle (..))
import Moe.Parsing.Anime (parseFilename)
import Moe.Parsing.Types (ParseResult (..))
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Test Data Types
--------------------------------------------------------------------------------

data TestCase = TestCase
  { description :: Text
  , input :: Text
  , expected :: ExpectedResult
  }
  deriving stock (Show)

data ExpectedResult = ExpectedResult
  { nameEn :: Maybe Text
  , nameZh :: Maybe Text
  , nameJp :: Maybe Text
  , episode :: Maybe Int
  , season :: Maybe Int
  , subtitleGroup :: Maybe Text
  , resolution :: Maybe Text
  , subtitleLanguage :: [Text]
  }
  deriving stock (Show)

newtype TestCases = TestCases {successCases :: [TestCase]}
  deriving stock (Show)

--------------------------------------------------------------------------------
-- JSON Parsing
--------------------------------------------------------------------------------

instance FromJSON TestCase where
  parseJSON = withObject "TestCase" $ \v ->
    TestCase
      <$> v .: "description"
      <*> v .: "input"
      <*> v .: "expected"

instance FromJSON ExpectedResult where
  parseJSON = withObject "ExpectedResult" $ \v ->
    ExpectedResult
      <$> v .:? "name_en"
      <*> v .:? "name_zh"
      <*> v .:? "name_jp"
      <*> v .:? "episode"
      <*> v .:? "season"
      <*> v .:? "subtitle_group"
      <*> v .:? "resolution"
      <*> v .:? "subtitle_language" .!= []

instance FromJSON TestCases where
  parseJSON = withObject "TestCases" $ \v ->
    TestCases <$> v .: "success_cases"

--------------------------------------------------------------------------------
-- Subtitle Language Mapping
--------------------------------------------------------------------------------

-- | Map JSON subtitle language strings to SubtitleLanguage
mapSubtitleLanguage :: Text -> Maybe Subtitle
mapSubtitleLanguage t = case T.toUpper t of
  "CHS" -> Just CHS
  "CHT" -> Just CHT
  "JPN" -> Just JAP
  "JAP" -> Just JAP
  _ -> Nothing

mapSubtitleLanguages :: [Text] -> [Subtitle]
mapSubtitleLanguages = foldr (\t acc -> maybe acc (: acc) (mapSubtitleLanguage t)) []

--------------------------------------------------------------------------------
-- Test Generation
--------------------------------------------------------------------------------

-- | Generate a single test case
makeTest :: TestCase -> TestTree
makeTest tc = testCase (T.unpack tc.description) $ do
  let result = parseFilename tc.input
  case result of
    Nothing -> assertFailure $ "Parser returned Nothing for: " <> T.unpack tc.input
    Just pr -> do
      pr.nameEnglish @?= tc.expected.nameEn
      pr.nameChinese @?= tc.expected.nameZh
      pr.nameJapanese @?= tc.expected.nameJp
      pr.episode @?= tc.expected.episode
      pr.season @?= tc.expected.season
      pr.subtitleGroup @?= tc.expected.subtitleGroup
      pr.resolution @?= tc.expected.resolution
      -- Compare sorted lists to ignore order differences
      sort pr.subtitles @?= sort (mapSubtitleLanguages tc.expected.subtitleLanguage)

-- | Load test cases from JSON file
loadTestCases :: FilePath -> IO TestCases
loadTestCases path = do
  content <- BL.readFile path
  case eitherDecode content of
    Left err -> error $ "Failed to parse test cases JSON: " <> toText err
    Right tc -> pure tc

--------------------------------------------------------------------------------
-- Test Tree
--------------------------------------------------------------------------------

-- | All parsing tests
tests :: IO TestTree
tests = do
  testCases <- loadTestCases "test/Moe/Parsing/test_cases.json"
  pure $
    testGroup
      "Moe.Parsing.Anime"
      [ testGroup "parseFilename" $
          map makeTest testCases.successCases
      ]
