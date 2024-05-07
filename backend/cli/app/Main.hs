module Main where

import "base" Control.Monad ( when )
import "aeson" Data.Aeson hiding ( Options )
import "base" Data.Version ( showVersion )
import "hasura-to-icf" Hasa.App ( run )
import "hasura-to-icf" Hasa.Types.Hasura ( MetadataV3 )
import "yaml-includes" Metadelta.HasuraYaml ( loadHasuraMetadataFromFolder )
import "optparse-generic" Options.Generic
import Paths_metadelta_cli ( version )
import "base" System.Environment ( getArgs )
import "base" System.Exit ( exitSuccess )


fromResult :: a -> Result a -> a
fromResult a r
  = case r of
      Success a' -> a'
      Error _    -> a


loadFromFolder :: FilePath -> IO MetadataV3
loadFromFolder p = do
  m <- fromJSON <$> loadHasuraMetadataFromFolder p
  pure $ fromResult (error $ "Error loading metadata at path: " ++ p) m


main :: IO ()
main = do
  args <- getArgs
  when ("--version" `elem` args) $ do
    putStrLn (showVersion version) >> exitSuccess
  unwrapRecord "metadelta" >>= run loadFromFolder
