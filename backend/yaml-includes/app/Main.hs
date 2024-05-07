module Main where

import "aeson" Data.Aeson qualified as Aeson
import "bytestring" Data.ByteString.Lazy.Char8 qualified as BS
import "yaml-includes" Metadelta.HasuraYaml ( loadHasuraMetadataFromFolder )
import "optparse-applicative" Options.Applicative


data Options
  = Options
      { folder :: String
      }


options :: Parser Options
options = Options
  <$> strOption
      ( long "folder"
      <> short 'f'
      <> metavar "FOLDER"
      <> help "The folder with the Hasura metadata."
      )


parser :: ParserInfo Options
parser =
  info (options <**> helper)
       (  fullDesc
       <> progDesc "Build a JSON file of the Hasura metadata."
       <> header   "Load Hasura Metadata"
       )


main :: IO ()
main = do
  opts <- execParser parser
  r <- loadHasuraMetadataFromFolder (folder opts)
  putStrLn $ BS.unpack $ Aeson.encode r
