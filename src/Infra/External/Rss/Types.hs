{- | RSS types for item parsing

This module defines the core types for RSS parsing:

- 'RawItem': Raw RSS item data extracted from XML
- 'RssError': Errors that can occur during RSS operations
-}
module Infra.External.Rss.Types (
  -- * Raw Item
  RawItem (..),

  -- * Errors
  RssError (..),
)
where

import Data.Text.Display (Display (..))

{- | Raw RSS item data extracted from XML

This contains all common RSS item fields as optional values.

Source-specific extensions are extracted from XML namespaces:

- @nyaaInfoHash@: From @\<nyaa:infoHash\>@ element (Nyaa.si)
- @nyaaCategory@: From @\<nyaa:category\>@ element (Nyaa.si)
-}
data RawItem = RawItem
  { title :: Maybe Text
  , link :: Maybe Text
  , description :: Maybe Text
  , pubDate :: Maybe Text
  , enclosure :: Maybe Text
  -- Source-specific extensions
  , nyaaInfoHash :: Maybe Text
  -- ^ Info hash from Nyaa's XML namespace @\<nyaa:infoHash\>@
  , nyaaCategory :: Maybe Text
  -- ^ Category from Nyaa's XML namespace @\<nyaa:category\>@
  }
  deriving stock (Show, Eq)

-- | Errors that can occur during RSS operations
data RssError
  = -- | Network request failed
    NetworkError Text
  | -- | Failed to parse XML
    XmlParseError Text
  deriving stock (Show, Eq)

instance Display RssError where
  displayBuilder (NetworkError msg) = "Network error: " <> displayBuilder msg
  displayBuilder (XmlParseError msg) = "XML parse error: " <> displayBuilder msg
