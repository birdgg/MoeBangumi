-- | RSS client using xml-conduit for XML parsing
--
-- This module provides functions to fetch and parse RSS feeds.
--
-- == Usage
--
-- @
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Infra.External.Rss.Client
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   result <- fetchRss manager "https://example.com/rss"
--   case result of
--     Right items -> print items
--     Left err -> print err
-- @
module Infra.External.Rss.Client
  ( -- * Fetching
    fetchRss

    -- * Parsing (pure)
  , parseRss

    -- * Re-exports
  , module Infra.External.Rss.Types
  )
where

import Control.Exception (try)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Infra.External.Rss.Types
import Network.HTTP.Client
import Network.HTTP.Types.Status qualified as Status
import Text.XML (Name (..), parseLBS, def)
import Text.XML.Cursor

-- | Fetch and parse RSS feed, returning raw items
fetchRss :: Manager -> Text -> IO (Either RssError [RawItem])
fetchRss manager url = do
  result <- fetchRssXml manager url
  pure $ result >>= parseRss

-- | Parse RSS XML into raw items (pure function)
parseRss :: ByteString -> Either RssError [RawItem]
parseRss xml = parseRssItems (LBS.fromStrict xml)

-- | Fetch raw RSS XML from URL
fetchRssXml :: Manager -> Text -> IO (Either RssError ByteString)
fetchRssXml manager url = do
  result <- try $ do
    request <- parseRequest (T.unpack url)
    let request' =
          request
            { requestHeaders =
                [ ("User-Agent", "moe-bangumi/rss-client")
                , ("Accept", "application/rss+xml, application/xml, text/xml")
                ]
            }
    response <- httpLbs request' manager
    pure (Status.statusCode (responseStatus response), responseBody response)
  case result of
    Left (e :: HttpException) ->
      pure $ Left $ NetworkError $ T.pack $ show e
    Right (status, body)
      | status >= 200 && status < 300 ->
          pure $ Right $ LBS.toStrict body
      | otherwise ->
          pure $ Left $ NetworkError $ "HTTP " <> T.pack (show status)

-- | Parse RSS items from XML using xml-conduit
--
-- Extracts all <item> elements from the RSS channel.
parseRssItems :: LBS.ByteString -> Either RssError [RawItem]
parseRssItems xml =
  case parseLBS def xml of
    Left err -> Left $ XmlParseError $ T.pack $ show err
    Right doc ->
      let cursor = fromDocument doc
          items = cursor $// element "item"
       in Right $ map parseItem items

-- | Parse a single RSS item from a cursor
parseItem :: Cursor -> RawItem
parseItem cursor =
  RawItem
    { title = getChildText "title" cursor
    , link = getChildText "link" cursor
    , description = getChildText "description" cursor
    , pubDate = getChildText "pubDate" cursor
    , enclosure = getEnclosureUrl cursor
    -- Nyaa namespace extensions
    , nyaaInfoHash = getNyaaText "infoHash" cursor
    , nyaaCategory = getNyaaText "category" cursor
    }

-- | Get text content of a child element
getChildText :: Text -> Cursor -> Maybe Text
getChildText name cursor =
  case cursor $/ element (fromText name) &/ content of
    [] -> Nothing
    (t : _) -> Just t
  where
    fromText :: Text -> Name
    fromText t = Name t Nothing Nothing

-- | Get the url attribute from enclosure element
getEnclosureUrl :: Cursor -> Maybe Text
getEnclosureUrl cursor =
  case cursor $/ element "enclosure" of
    [] -> Nothing
    (enc : _) -> listToMaybe $ attribute "url" enc

-- | Get text content from Nyaa namespace element
--
-- Nyaa.si RSS uses namespace: @xmlns:nyaa="https://nyaa.si/xmlns/nyaa"@
-- Elements like @\<nyaa:infoHash\>@ contain the info hash directly.
getNyaaText :: Text -> Cursor -> Maybe Text
getNyaaText localName cursor =
  case cursor $/ element nyaaName &/ content of
    [] -> Nothing
    (t : _) -> Just t
  where
    -- Nyaa namespace URI
    nyaaNs :: Maybe Text
    nyaaNs = Just "https://nyaa.si/xmlns/nyaa"

    nyaaName :: Name
    nyaaName = Name localName nyaaNs Nothing
