-- | Tests for collection/batch torrent parsing
-- Using real VCB-Studio Re:Zero releases as test cases
module Moe.Parsing.CollectionSpec (tests) where

import Moe.Parsing.Collection
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

--------------------------------------------------------------------------------
-- Test Data: VCB-Studio Re:Zero Releases
--------------------------------------------------------------------------------

-- | Season 2 TV series
vcbReZeroS2 :: Text
vcbReZeroS2 = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [Ma10p_1080p]"

-- | OVA: Hyouketsu no Kizuna (Frozen Bond)
vcbReZeroOVA1 :: Text
vcbReZeroOVA1 = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Hyouketsu no Kizuna [Ma10p_1080p]"

-- | OVA: Memory Snow
vcbReZeroOVA2 :: Text
vcbReZeroOVA2 = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Memory Snow [Ma10p_1080p]"

-- | Director's Cut version
vcbReZeroDirectorsCut :: Text
vcbReZeroDirectorsCut = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Shin Henshuu-ban [Ma10p_1080p]"

-- | Season 1 TV series
vcbReZeroS1 :: Text
vcbReZeroS1 = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu [Ma10p_1080p]"

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Moe.Parsing.Collection"
    [ testGroup "VCB-Studio Re:Zero - Content Type Detection" vcbContentTypeTests
    , testGroup "VCB-Studio Re:Zero - Collection Info Parsing" vcbCollectionInfoTests
    , testGroup "Content Type Detection" contentTypeTests
    , testGroup "Season Extraction from Path" seasonPathTests
    , testGroup "Special Directory Detection" specialDirTests
    ]

--------------------------------------------------------------------------------
-- VCB-Studio Content Type Detection Tests
--------------------------------------------------------------------------------

vcbContentTypeTests :: [TestTree]
vcbContentTypeTests =
  [ testCase "Re:Zero S2 with 25 episodes -> Collection" $ do
      let files :: [Text]
          files = map (\n -> "Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [" <> show n <> "].mkv") [1..25 :: Int]
      detectContentType files @?= Collection

  , testCase "Re:Zero S1 with 25 episodes -> Collection" $ do
      let files :: [Text]
          files = map (\n -> "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu [" <> show n <> "].mkv") [1..25 :: Int]
      detectContentType files @?= Collection

  , testCase "Re:Zero OVA single file -> SingleEpisode" $ do
      let files = ["[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Memory Snow [Ma10p_1080p].mkv"]
      detectContentType files @?= SingleEpisode

  , testCase "Re:Zero Director's Cut with SPs -> Collection" $ do
      let files =
            [ "01.mkv", "02.mkv", "03.mkv", "04.mkv", "05.mkv"
            , "06.mkv", "07.mkv", "08.mkv", "09.mkv", "10.mkv"
            , "11.mkv", "12.mkv", "13.mkv"
            , "SPs/SP01.mkv", "SPs/SP02.mkv"
            ]
      detectContentType files @?= Collection

  , testCase "Re:Zero with CDs and Scans subdirs -> Collection (video only)" $ do
      -- VCB releases often have CDs, Scans, etc. subdirs
      let files =
            [ "01.mkv", "02.mkv", "03.mkv"
            , "CDs/OST/01.flac", "CDs/OST/02.flac"
            , "Scans/cover.jpg"
            ]
      detectContentType files @?= Collection  -- Only counts video files
  ]

--------------------------------------------------------------------------------
-- VCB-Studio Collection Info Parsing Tests
--------------------------------------------------------------------------------

vcbCollectionInfoTests :: [TestTree]
vcbCollectionInfoTests =
  [ testCase "Re:Zero S2 - parses as Collection" $ do
      let files = ["01.mkv", "02.mkv", "03.mkv"]
          info = parseCollectionInfo vcbReZeroS2 files
      info.contentType @?= Collection

  , testCase "Re:Zero S1 - parses as Collection" $ do
      let files = ["01.mkv", "02.mkv", "03.mkv"]
          info = parseCollectionInfo vcbReZeroS1 files
      info.contentType @?= Collection

  , testCase "Re:Zero OVA Memory Snow - single file" $ do
      let files = ["Memory Snow.mkv"]
          info = parseCollectionInfo vcbReZeroOVA2 files
      info.contentType @?= SingleEpisode

  , testCase "Re:Zero OVA Hyouketsu no Kizuna - single file" $ do
      let files = ["Hyouketsu no Kizuna.mkv"]
          info = parseCollectionInfo vcbReZeroOVA1 files
      info.contentType @?= SingleEpisode

  , testCase "Re:Zero Director's Cut - parses as Collection" $ do
      let files = ["01.mkv", "02.mkv", "03.mkv", "04.mkv", "05.mkv"]
          info = parseCollectionInfo vcbReZeroDirectorsCut files
      info.contentType @?= Collection

  , testCase "VCB torrent name - no episode range" $ do
      -- VCB doesn't use [01-25] style episode ranges
      let info = parseCollectionInfo vcbReZeroS1 ["01.mkv"]
      info.episodeRange @?= Nothing

  , testCase "VCB torrent name - title extraction" $ do
      let info = parseCollectionInfo vcbReZeroS1 ["01.mkv"]
      -- parseResult should contain parsed info
      isJust info.parseResult @?= True
  ]

--------------------------------------------------------------------------------
-- Content Type Detection Tests
--------------------------------------------------------------------------------

contentTypeTests :: [TestTree]
contentTypeTests =
  [ testCase "Single video file -> SingleEpisode" $ do
      detectContentType ["[Group] Anime - 01.mkv"] @?= SingleEpisode

  , testCase "Empty file list -> SingleEpisode" $ do
      detectContentType ([] :: [Text]) @?= SingleEpisode

  , testCase "Multiple episodes in root -> Collection" $ do
      detectContentType ["EP01.mkv", "EP02.mkv", "EP03.mkv"] @?= Collection

  , testCase "Multiple season directories (S1, S2) -> MultiSeasonCollection" $ do
      let files =
            [ "[Group][Title][S1]/EP01.mkv"
            , "[Group][Title][S1]/EP02.mkv"
            , "[Group][Title][S2]/EP01.mkv"
            , "[Group][Title][S2]/EP02.mkv"
            ]
      detectContentType files @?= MultiSeasonCollection

  , testCase "Ignores non-video files" $ do
      detectContentType ["video.mkv", "sub.srt", "cover.jpg", "nfo.txt"] @?= SingleEpisode
  ]

--------------------------------------------------------------------------------
-- Season Extraction from Path Tests
--------------------------------------------------------------------------------

seasonPathTests :: [TestTree]
seasonPathTests =
  [ testCase "S1 pattern" $
      extractSeasonFromPath "[Group][Title][S1]/EP01.mkv" @?= Just 1

  , testCase "S02 pattern" $
      extractSeasonFromPath "[Group][Title][S02]/EP05.mkv" @?= Just 2

  , testCase "Season 1 pattern" $
      extractSeasonFromPath "Season 1/EP01.mkv" @?= Just 1

  , testCase "SP directory -> 0 (specials)" $
      extractSeasonFromPath "[Group][Title]/SP/SP01.mkv" @?= Just 0

  , testCase "SPs directory -> 0 (specials)" $
      extractSeasonFromPath "SPs/SP01.mkv" @?= Just 0

  , testCase "Special directory -> 0" $
      extractSeasonFromPath "[Group][Title]/Special/OVA01.mkv" @?= Just 0

  , testCase "OVA directory -> 0" $
      extractSeasonFromPath "[Group][Title]/OVA/OVA1.mkv" @?= Just 0

  , testCase "No season indicator -> Nothing" $
      extractSeasonFromPath "EP01.mkv" @?= Nothing
  ]

--------------------------------------------------------------------------------
-- Special Directory Detection Tests
--------------------------------------------------------------------------------

specialDirTests :: [TestTree]
specialDirTests =
  [ testCase "SP is special" $
      isSpecialDirectory "SP" @?= True

  , testCase "SPs is special" $
      isSpecialDirectory "SPs" @?= True

  , testCase "sp (lowercase) is special" $
      isSpecialDirectory "sp" @?= True

  , testCase "Special is special" $
      isSpecialDirectory "Special" @?= True

  , testCase "OVA is special" $
      isSpecialDirectory "OVA" @?= True

  , testCase "OAD is special" $
      isSpecialDirectory "OAD" @?= True

  , testCase "Extras is special" $
      isSpecialDirectory "Extras" @?= True

  , testCase "NCOP is special" $
      isSpecialDirectory "NCOP" @?= True

  , testCase "Menu is special" $
      isSpecialDirectory "Menu" @?= True

  , testCase "CDs is not special" $
      isSpecialDirectory "CDs" @?= False

  , testCase "Scans is not special" $
      isSpecialDirectory "Scans" @?= False

  , testCase "Season 1 is not special" $
      isSpecialDirectory "Season 1" @?= False
  ]
