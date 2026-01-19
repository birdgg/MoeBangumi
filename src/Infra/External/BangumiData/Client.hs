{- | Bangumi-data client for fetching calendar data from GitHub

Fetches and parses JSON data from the bangumi-data repository
to extract seasonal anime calendar with mikan_id, bgmtv_id, tmdb_id mappings.

Usage:

@
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Infra.External.BangumiData.Client

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  result <- fetchCalendar manager 2025 Winter
  case result of
    Right items -> mapM_ print items
    Left err -> print err
@
-}
module Infra.External.BangumiData.Client (
  -- * Fetching
  fetchCalendar,
  findByBgmtvId,

  -- * URLs
  bangumiDataBaseUrl,

  -- * Re-exports
  module Infra.External.BangumiData.Types,
) where

import Control.Exception (try)
import Data.Aeson (eitherDecodeStrict)
import Data.Text qualified as T
import Data.Time (Day, DayOfWeek (..), dayOfWeek, toGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Infra.External.BangumiData.Types
import Moe.Calendar (Season (..))
import Moe.Util (padNumberStart)
import Network.HTTP.Client
import Network.HTTP.Types.Status qualified as Status

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | jsDelivr CDN base URL (faster in China, uses GitHub repo)
bangumiDataBaseUrl :: Text
bangumiDataBaseUrl = "https://cdn.jsdelivr.net/gh/bangumi-data/bangumi-data@master/data/items"

--------------------------------------------------------------------------------
-- URL Building
--------------------------------------------------------------------------------

{- | Build URL for specific year and season
bangumi-data organizes files by year/month, where month corresponds to season start
-}
bangumiDataUrl :: Int -> Season -> Text
bangumiDataUrl year season =
  bangumiDataBaseUrl
    <> "/"
    <> show year
    <> "/"
    <> toText (padNumberStart (seasonToMonth season))
    <> ".json"
 where
  seasonToMonth :: Season -> Int
  seasonToMonth Winter = 1
  seasonToMonth Spring = 4
  seasonToMonth Summer = 7
  seasonToMonth Fall = 10

--------------------------------------------------------------------------------
-- Fetching
--------------------------------------------------------------------------------

-- | Fetch calendar data for a specific year and season
fetchCalendar ::
  Manager ->
  Int ->
  Season ->
  IO (Either BangumiDataError [CalendarItem])
fetchCalendar manager year season = do
  let url = bangumiDataUrl year season
  result <- fetchJson manager url
  pure $ result >>= parseAndExtract

-- | Find a bangumi-data entry by BGM.tv subject ID and air date
-- Uses airdate to determine which seasonal JSON file to fetch,
-- then searches for matching bgmtv_id within that season's data.
findByBgmtvId ::
  Manager ->
  -- | BGM.tv subject ID
  Int ->
  -- | Air date in YYYY-MM-DD format
  Text ->
  IO (Either BangumiDataError (Maybe CalendarItem))
findByBgmtvId manager bgmtvIdParam airdate = do
  case parseAirDate airdate of
    Nothing -> pure $ Left $ JsonParseError $ "Invalid airdate format: " <> airdate
    Just (year, season) -> do
      result <- fetchCalendar manager year season
      pure $ fmap (find matchesBgmtvId) result
 where
  matchesBgmtvId :: CalendarItem -> Bool
  matchesBgmtvId item = item.bgmtvId == Just bgmtvIdParam

-- | Parse airdate (YYYY-MM-DD) to year and season
parseAirDate :: Text -> Maybe (Int, Season)
parseAirDate dateStr = do
  day <- parseDateString dateStr
  let (year, month, _) = toGregorian day
  pure (fromInteger year, monthToSeason month)
 where
  monthToSeason :: Int -> Season
  monthToSeason m
    | m >= 1 && m <= 3 = Winter
    | m >= 4 && m <= 6 = Spring
    | m >= 7 && m <= 9 = Summer
    | otherwise = Fall

-- | Fetch JSON from URL
fetchJson :: Manager -> Text -> IO (Either BangumiDataError ByteString)
fetchJson manager url = do
  result <- try $ do
    request <- parseRequest (T.unpack url)
    let request' =
          request
            { requestHeaders =
                [ ("User-Agent", "moe-bangumi/bangumi-data-client")
                , ("Accept", "application/json")
                ]
            }
    response <- httpLbs request' manager
    pure (Status.statusCode (responseStatus response), responseBody response)
  case result of
    Left (e :: HttpException) ->
      pure $ Left $ NetworkError $ show e
    Right (status, body)
      | status >= 200 && status < 300 ->
          pure $ Right $ toStrict body
      | otherwise ->
          pure $ Left $ NetworkError $ "HTTP " <> show status

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse JSON and extract calendar items
parseAndExtract :: ByteString -> Either BangumiDataError [CalendarItem]
parseAndExtract bs =
  case eitherDecodeStrict bs of
    Left err -> Left $ JsonParseError $ show err
    Right items -> Right $ map toCalendarItem items

-- | Convert BangumiDataItem to CalendarItem
toCalendarItem :: BangumiDataItem -> CalendarItem
toCalendarItem item =
  CalendarItem
    { mikanId = findSiteId "mikan" item.sites
    , bgmtvId = findSiteId "bangumi" item.sites >>= readMaybe . toString
    , tmdbId = findSiteId "tmdb" item.sites >>= parseTmdbId
    , titleChinese = extractChineseTitle item.titleTranslate item.title
    , titleJapanese = item.title
    , officialSite = item.officialSite
    , airDate = Just $ extractDate item.begin
    , airWeekday = extractWeekday item.broadcast
    , itemType = item.itemType
    }

--------------------------------------------------------------------------------
-- ID Extraction
--------------------------------------------------------------------------------

-- | Find site ID from sites array
findSiteId :: Text -> [Site] -> Maybe Text
findSiteId siteName sites =
  listToMaybe [s.siteId | s <- sites, s.site == siteName]

-- | Parse TMDB ID from "tv/274580" or "tv/274580/season/1" format
parseTmdbId :: Text -> Maybe Int
parseTmdbId t =
  case T.splitOn "/" t of
    ["tv", idStr] -> readMaybe $ toString idStr
    ["tv", idStr, "season", _] -> readMaybe $ toString idStr
    _ -> Nothing

--------------------------------------------------------------------------------
-- Title Extraction
--------------------------------------------------------------------------------

-- | Extract Chinese title (prefer zh-Hans, then zh-Hant, fallback to original)
extractChineseTitle :: TitleTranslate -> Text -> Text
extractChineseTitle trans fallback =
  fromMaybe fallback $
    (trans.zhHans >>= listToMaybe)
      <|> (trans.zhHant >>= listToMaybe)

--------------------------------------------------------------------------------
-- Date/Time Extraction
--------------------------------------------------------------------------------

-- | Parse YYYY-MM-DD format date string to Day
parseDateString :: Text -> Maybe Day
parseDateString dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%d" (toString dateStr)

-- | Extract date from ISO8601 timestamp: "2025-01-02T14:00:00.000Z" -> "2025-01-02"
extractDate :: Text -> Text
extractDate = T.take 10

{- | Extract weekday from broadcast recurrence rule
"R/2025-01-02T14:00:00.000Z/P7D" -> parse date and get weekday
-}
extractWeekday :: Maybe Text -> Int
extractWeekday Nothing = 0
extractWeekday (Just broadcast) =
  case T.splitOn "/" broadcast of
    ["R", dateStr, _] ->
      let date = T.take 10 dateStr
       in parseWeekdayFromDate date
    _ -> 0
 where
  parseWeekdayFromDate :: Text -> Int
  parseWeekdayFromDate dateStr =
    case parseDateString dateStr of
      Just day -> dayOfWeekToInt $ dayOfWeek day
      Nothing -> 0

  -- Convert to 1=Monday...7=Sunday (ISO 8601 weekday numbering)
  dayOfWeekToInt :: DayOfWeek -> Int
  dayOfWeekToInt Monday = 1
  dayOfWeekToInt Tuesday = 2
  dayOfWeekToInt Wednesday = 3
  dayOfWeekToInt Thursday = 4
  dayOfWeekToInt Friday = 5
  dayOfWeekToInt Saturday = 6
  dayOfWeekToInt Sunday = 7
