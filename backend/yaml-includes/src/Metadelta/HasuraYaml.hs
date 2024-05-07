module Metadelta.HasuraYaml where

import "aeson" Data.Aeson qualified as Aeson
import "aeson" Data.Aeson.KeyMap ( fromList )
import "text" Data.Text ( isPrefixOf, unpack )
import "text" Data.Text qualified as Text
import "yaml" Data.Yaml ( Value (..), decodeFileThrow )
import "base" GHC.Generics ( Generic )
import "filepath" System.FilePath ( takeDirectory, (</>) )


data Version
  = Version
      { version :: Int
      }
  deriving (Generic)
  deriving anyclass (Aeson.FromJSON)


loadHasuraMetadataFromFolder :: FilePath -> IO Value
loadHasuraMetadataFromFolder dir = do
  -- Two steps:
  --
  -- 1. Load the "version.yaml" for the version,
  v <- decodeFileThrow @_ @Version (dir </> "version.yaml")

  -- 2. Load "sources"
  d <- decodeFileThrow @_ @Value (dir </> "databases/databases.yaml")
  sources <- resolveIncludes (dir </> "databases") d

  -- That's it!
  let toInt = Number . fromIntegral
  pure $ Object $ fromList [ ("version", toInt $ version v)
                           , ("sources", sources)
                           ]


-- | Resolve the special `!include` directive as just another yaml file to
-- load in-place.
resolveIncludes :: FilePath -> Value -> IO Value
resolveIncludes dir v@(String s) =
  let prefix = "!include "
  in if isPrefixOf prefix s
        then loadIncluded dir $ unpack $ Text.drop (Text.length prefix) s
        else pure v
resolveIncludes dir (Array  xs) = Array  <$> traverse (resolveIncludes dir) xs
resolveIncludes dir (Object o)  = Object <$> traverse (resolveIncludes dir) o
resolveIncludes _ x = pure x


loadIncluded :: FilePath -> FilePath -> IO Value
loadIncluded dir f = resolveIncludes newDir =<< decodeFileThrow f'
  where
    f'     = dir </> f
    newDir = takeDirectory f'
