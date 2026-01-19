{- | Emby API types

This module defines the data types for Emby API responses.
-}
module Infra.External.Emby.Types
  ( -- * Library
    EmbyLibrary (..)

    -- * Items
  , EmbyItem (..)
  , EmbyItemsResponse (..)
  , EmbyProviderIds (..)

    -- * Error
  , EmbyError (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text.Display (Display (..))

--------------------------------------------------------------------------------
-- Library
--------------------------------------------------------------------------------

-- | Emby virtual folder (library)
data EmbyLibrary = EmbyLibrary
  { name :: Text
  , collectionType :: Maybe Text
  , itemId :: Maybe Text
  , locations :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EmbyLibrary where
  parseJSON = withObject "EmbyLibrary" $ \o ->
    EmbyLibrary
      <$> o .: "Name"
      <*> o .:? "CollectionType"
      <*> o .:? "ItemId"
      <*> o .: "Locations"

instance ToJSON EmbyLibrary where
  toJSON lib =
    object
      [ "Name" .= lib.name
      , "CollectionType" .= lib.collectionType
      , "ItemId" .= lib.itemId
      , "Locations" .= lib.locations
      ]

--------------------------------------------------------------------------------
-- Items
--------------------------------------------------------------------------------

-- | Emby provider IDs (external service IDs)
data EmbyProviderIds = EmbyProviderIds
  { tmdb :: Maybe Text
  , tvdb :: Maybe Text
  , imdb :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EmbyProviderIds where
  parseJSON = withObject "EmbyProviderIds" $ \o ->
    EmbyProviderIds
      <$> o .:? "Tmdb"
      <*> o .:? "Tvdb"
      <*> o .:? "Imdb"

instance ToJSON EmbyProviderIds where
  toJSON p =
    object
      [ "Tmdb" .= p.tmdb
      , "Tvdb" .= p.tvdb
      , "Imdb" .= p.imdb
      ]

-- | Emby media item
data EmbyItem = EmbyItem
  { id :: Text
  , name :: Text
  , type_ :: Text
  , providerIds :: Maybe EmbyProviderIds
  , overview :: Maybe Text
  , premiereDate :: Maybe Text
  , productionYear :: Maybe Int
  , childCount :: Maybe Int
  , imageTags :: Maybe (Map Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EmbyItem where
  parseJSON = withObject "EmbyItem" $ \o ->
    EmbyItem
      <$> o .: "Id"
      <*> o .: "Name"
      <*> o .: "Type"
      <*> o .:? "ProviderIds"
      <*> o .:? "Overview"
      <*> o .:? "PremiereDate"
      <*> o .:? "ProductionYear"
      <*> o .:? "ChildCount"
      <*> o .:? "ImageTags"

instance ToJSON EmbyItem where
  toJSON item =
    object
      [ "Id" .= item.id
      , "Name" .= item.name
      , "Type" .= item.type_
      , "ProviderIds" .= item.providerIds
      , "Overview" .= item.overview
      , "PremiereDate" .= item.premiereDate
      , "ProductionYear" .= item.productionYear
      , "ChildCount" .= item.childCount
      , "ImageTags" .= item.imageTags
      ]

-- | Emby items query response
data EmbyItemsResponse = EmbyItemsResponse
  { items :: [EmbyItem]
  , totalRecordCount :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EmbyItemsResponse where
  parseJSON = withObject "EmbyItemsResponse" $ \o ->
    EmbyItemsResponse
      <$> o .: "Items"
      <*> o .: "TotalRecordCount"

instance ToJSON EmbyItemsResponse where
  toJSON resp =
    object
      [ "Items" .= resp.items
      , "TotalRecordCount" .= resp.totalRecordCount
      ]

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

-- | Emby API error
data EmbyError
  = EmbyConnectionError Text
  | EmbyApiError Text
  | EmbyParseError Text
  | EmbyDisabled
  deriving stock (Show, Eq)

instance Display EmbyError where
  displayBuilder = \case
    EmbyConnectionError msg -> "Emby connection error: " <> displayBuilder msg
    EmbyApiError msg -> "Emby API error: " <> displayBuilder msg
    EmbyParseError msg -> "Emby parse error: " <> displayBuilder msg
    EmbyDisabled -> "Emby integration is disabled"
