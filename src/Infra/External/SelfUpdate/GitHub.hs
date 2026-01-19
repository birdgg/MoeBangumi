-- | GitHub Releases API client for self-update functionality
module Infra.External.SelfUpdate.GitHub
  ( -- * API Functions
    fetchReleases
  , fetchLatestRelease
  , downloadAsset

    -- * Asset Matching
  , findMatchingAsset
  , getTargetTriple
  )
where

import Control.Exception (try)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Infra.External.SelfUpdate.Types
import System.Info (arch, os)

-- | GitHub release response from API (internal type)
data GitHubRelease = GitHubRelease
  { tagName :: Text
  , assets :: [GitHubAsset]
  , htmlUrl :: Text
  , body :: Maybe Text
  }

instance FromJSON GitHubRelease where
  parseJSON = withObject "GitHubRelease" $ \o ->
    GitHubRelease
      <$> o .: "tag_name"
      <*> o .: "assets"
      <*> o .: "html_url"
      <*> o .:? "body"

-- | GitHub asset response from API (internal type)
data GitHubAsset = GitHubAsset
  { name :: Text
  , browserDownloadUrl :: Text
  , size :: Int
  , contentType :: Text
  }

instance FromJSON GitHubAsset where
  parseJSON = withObject "GitHubAsset" $ \o ->
    GitHubAsset
      <$> o .: "name"
      <*> o .: "browser_download_url"
      <*> o .: "size"
      <*> o .: "content_type"

-- | Convert GitHub release to our Release type
toRelease :: GitHubRelease -> Release
toRelease gh =
  Release
    { version = gh.tagName
    , assets = map toReleaseAsset gh.assets
    , htmlUrl = gh.htmlUrl
    , body = gh.body
    }

toReleaseAsset :: GitHubAsset -> ReleaseAsset
toReleaseAsset ga =
  ReleaseAsset
    { name = ga.name
    , downloadUrl = ga.browserDownloadUrl
    , size = ga.size
    , contentType = ga.contentType
    }

-- | Make a GitHub API request
makeGitHubRequest
  :: (FromJSON a)
  => Manager
  -> UpdateConfig
  -> String
  -> Text
  -> (a -> b)
  -> IO (Either UpdateError b)
makeGitHubRequest manager config url notFoundMsg transform = do
  result <- try $ do
    request <- parseRequest url
    let request' = addHeaders config request
    response <- httpLbs request' manager
    pure (statusCode (responseStatus response), responseBody response)
  case result of
    Left (e :: HttpException) ->
      pure $ Left $ NetworkError $ T.pack $ show e
    Right (status, respBody)
      | status == 200 ->
          case Aeson.eitherDecode respBody of
            Left err -> pure $ Left $ ParseError $ T.pack err
            Right value -> pure $ Right $ transform value
      | status == 404 ->
          pure $ Left $ NetworkError notFoundMsg
      | otherwise ->
          pure $ Left $ NetworkError $ "HTTP " <> T.pack (show status)

-- | Fetch all releases from GitHub
fetchReleases :: Manager -> UpdateConfig -> IO (Either UpdateError [Release])
fetchReleases manager config =
  makeGitHubRequest
    manager
    config
    (repoUrl config <> "/releases")
    "Repository not found"
    (map toRelease)

-- | Fetch the latest release from GitHub
fetchLatestRelease :: Manager -> UpdateConfig -> IO (Either UpdateError Release)
fetchLatestRelease manager config =
  makeGitHubRequest
    manager
    config
    (repoUrl config <> "/releases/latest")
    "No releases found"
    toRelease

-- | Build the base repository URL
repoUrl :: UpdateConfig -> String
repoUrl config =
  "https://api.github.com/repos/"
    <> T.unpack config.repoOwner
    <> "/"
    <> T.unpack config.repoName

-- | Download an asset to a file
downloadAsset :: Manager -> UpdateConfig -> ReleaseAsset -> FilePath -> IO (Either UpdateError ())
downloadAsset manager config asset destPath = do
  result <- try $ do
    request <- parseRequest $ T.unpack asset.downloadUrl
    let request' =
          addHeaders config $
            request
              { requestHeaders =
                  ("Accept", "application/octet-stream") : requestHeaders request
              }
    response <- httpLbs request' manager
    pure (statusCode (responseStatus response), responseBody response)
  case result of
    Left (e :: HttpException) ->
      pure $ Left $ NetworkError $ T.pack $ show e
    Right (status, respBody)
      | status == 200 -> do
          LBS.writeFile destPath respBody
          pure $ Right ()
      | otherwise ->
          pure $ Left $ NetworkError $ "Download failed: HTTP " <> T.pack (show status)

-- | Add required headers for GitHub API
addHeaders :: UpdateConfig -> Request -> Request
addHeaders config req =
  req
    { requestHeaders =
        [ ("User-Agent", "self-update-haskell")
        , ("Accept", "application/vnd.github.v3+json")
        ]
          <> maybe [] (\t -> [("Authorization", "Bearer " <> TE.encodeUtf8 t)]) config.authToken
          <> requestHeaders req
    }

-- | Find a matching asset for the current platform
findMatchingAsset :: UpdateConfig -> Release -> Either UpdateError ReleaseAsset
findMatchingAsset config release = do
  let triple = fromMaybe getTargetTriple config.targetTriple
      patterns = assetPatterns config.binName triple
  case find (\a -> any (`T.isInfixOf` a.name) patterns) release.assets of
    Just asset -> Right asset
    Nothing ->
      Left $
        AssetNotFound $
          "No matching asset found for "
            <> config.binName
            <> " on "
            <> triple

-- | Generate possible asset name patterns based on target triple
assetPatterns :: Text -> Text -> [Text]
assetPatterns binName triple =
  [ binName <> "-" <> triple
  , binName <> "_" <> triple
  ]
    <> platformSpecificPatterns binName
  where
    platformSpecificPatterns bn
      | "linux" `T.isInfixOf` triple =
          [ bn <> "-linux-x86_64"
          , bn <> "_linux_x86_64"
          , bn <> "-linux-amd64"
          , bn <> "_linux_amd64"
          ]
      | "darwin" `T.isInfixOf` triple =
          [ bn <> "-darwin-x86_64"
          , bn <> "_darwin_x86_64"
          , bn <> "-macos-x86_64"
          , bn <> "_macos_x86_64"
          ]
      | otherwise = []

-- | Get the target triple for the current system
getTargetTriple :: Text
getTargetTriple =
  T.pack $ archStr <> "-" <> osStr
  where
    archStr = case arch of
      "x86_64" -> "x86_64"
      "aarch64" -> "aarch64"
      other -> other
    osStr = case os of
      "linux" -> "unknown-linux-gnu"
      "darwin" -> "apple-darwin"
      other -> other
