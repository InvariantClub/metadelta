module Hasa.App where

import "lens" Control.Lens hiding ( Unwrapped, Wrapped )
import "base" Control.Monad ( when )
import "aeson" Data.Aeson ( eitherDecodeFileStrict, encode )
import "generic-lens" Data.Generics.Labels ()
import "text" Data.Text ( Text )
import "text" Data.Text qualified as T
import "text" Data.Text.IO qualified as T
import "base" GHC.Generics ( Generic )
import Hasa.Conversion ( convert, toSingleSource, toText )
import Hasa.Diff ( metadataDiff )
import Hasa.Types.Frontend ( New (..), Old (..), Src (..), anythingChanged )
import Hasa.Types.Hasura ( MetadataFile, MetadataV3 )
import "optparse-generic" Options.Generic
  ( ParseRecord
  , Unwrapped
  , Wrapped
  , type (<#>)
  , type (<?>)
  , (:::)
  )
import "base" System.Exit ( ExitCode (ExitFailure), exitWith )
import "base" System.IO ( stderr )

-- | Command-line Options.
--
-- Note that we unfortunately have to do type-unsafe records here, but that's
-- just what optparse-generic requires. We only destructure the records
-- though, so it won't hurt us.
data Options w
  = Diff
      { oldPath  :: w ::: String <?> "The path of the 'old' metadata." <#> "o"
      , oldLabel :: w ::: Maybe Text <?> "An optional label."
      , oldLink  :: w ::: Maybe Text <?> "An optional link."
      , newPath  :: w ::: String <?> "The path to the 'new' metadata." <#> "n"
      , newLabel :: w ::: Maybe Text <?> "An optional label."
      , newLink  :: w ::: Maybe Text <?> "An optional link."
      }
  | Single
      { path  :: w ::: String <?> "The path to the metadata." <#> "p"
      , label :: w ::: Maybe Text <?> "An optional label."
      , link  :: w ::: Maybe Text <?> "An optional link."
      }
  deriving (Generic)


instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)


-- | Load metadata direct from a hasura metadata json file. We typically
-- obtain these by running `yaml-includes` on the directory; i.e. converting
-- the directory-structure-based YAML to JSON.
loadMetadata :: FilePath -> IO MetadataV3
loadMetadata path = do
  em <- eitherDecodeFileStrict @MetadataFile path
  case em of
    Left _ -> do
      em' <- eitherDecodeFileStrict path
      case em' of
        Left e   -> error $ show $ e
        Right md -> pure md
    Right m -> pure (m ^. #metadata)


runSingle :: (Src, MetadataV3) -> IO ()
runSingle (src, meta) = do
  let out = toSingleSource (convert meta) src
  T.putStrLn . toText . encode $ out


runDiff :: Old (Src, MetadataV3) -> New (Src, MetadataV3) -> IO ()
runDiff (Old (oldSrc, old)) (New (newSrc, new)) = do
  let diff = metadataDiff
                  (Old (oldSrc, old))
                  (New (newSrc, new))

  when (not $ anythingChanged diff) $ do
    T.hPutStrLn stderr
      $ "Error: No changed detected detected between "
        <> T.pack (show oldSrc)
        <> " and "
        <> T.pack (show newSrc)
        <> "."
    exitWith (ExitFailure 1)

  T.putStrLn . toText . encode $ diff


run :: (FilePath -> IO MetadataV3) -> Options Unwrapped -> IO ()
run load (Single {path, label, link}) = do
  meta <- load path
  let label' = maybe (T.pack path) id label
  let src = Src { link = link, label = label' }
  runSingle (src, meta)
run load (Diff {oldPath,oldLabel,oldLink,newPath,newLabel,newLink}) = do
  old <- load oldPath
  new <- load newPath
  let oldLabel' = maybe (T.pack oldPath) id oldLabel
      newLabel' = maybe (T.pack newPath) id newLabel

  let oldSrc = Src { link = oldLink, label = oldLabel' }
      newSrc = Src { link = newLink, label = newLabel' }

  runDiff (Old (oldSrc, old)) (New (newSrc, new))
