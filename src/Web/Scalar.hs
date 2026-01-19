module Web.Scalar (
  -- * Types
  HTML,
  RawHtml (..),

  -- * Handlers
  scalarHtml,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (..))

-- | Raw HTML type for Scalar documentation page
newtype RawHtml = RawHtml {unRawHtml :: LBS.ByteString}

-- | HTML content type
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRawHtml

-- | Generate Scalar API documentation HTML page
scalarHtml :: RawHtml
scalarHtml =
  RawHtml $
    LBS8.unlines
      [ "<!DOCTYPE html>"
      , "<html>"
      , "  <head>"
      , "    <title>Moe Bangumi API</title>"
      , "    <meta charset=\"utf-8\" />"
      , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
      , "  </head>"
      , "  <body>"
      , "    <script id=\"api-reference\" data-url=\"/docs/openapi.json\"></script>"
      , "    <script src=\"https://cdn.jsdelivr.net/npm/@scalar/api-reference\"></script>"
      , "  </body>"
      , "</html>"
      ]
