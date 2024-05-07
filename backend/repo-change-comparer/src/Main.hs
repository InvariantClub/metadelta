{-# language OverloadedStrings #-}

{-
Simple script to generate all the diffs on a particular repository, and
emit metadelta diff files for all of them.

Note that this relies on `yaml-includes` binary being present in the path (and
`git`).
-}

module Main where

import "text" Data.Text qualified as Text
import "optparse-applicative" Options.Applicative
import "base" Text.Printf qualified as Printf
import "turtle" Turtle hiding ( header, prefix )

data Options
  = Options
      { repo         :: String
      , metadataPath :: String
      , outputPath   :: String
      }


opts :: Parser Options
opts = Options
  <$> strOption
      ( long "repo"
      <> short 'r'
      <> metavar "REPO"
      <> help "The root of the repository."
      )
  <*> strOption
      ( long "metadata-path"
      <> short 'p'
      <> metavar "PATH"
      <> help "The relative path inside the folder where the hasura metadata lives."
      )
  <*> strOption
      ( long "output-path"
      <> short 'o'
      <> metavar "PATH"
      <> help "The folder to write our results to."
      <> value "output"
      )


parser :: ParserInfo Options
parser =
  info (opts <**> helper)
       (  fullDesc
       <> progDesc "Emit a list of json files representing the Hasura configurations of all relevant changes."
       <> header   "Repo Change Comparer."
       )


makeAbsolute :: FilePath -> FilePath -> FilePath
makeAbsolute cwd path =
  if isRelative path
     then cwd <> "/" <> path
     else path


main :: IO ()
main = do
  parsedOpts <- execParser parser

  let repoDir' = repo parsedOpts
      metaDir  = metadataPath parsedOpts
      outDir'  = outputPath parsedOpts

  sh $ do
    curDir <- pwd

    let repoDir = makeAbsolute curDir repoDir'
    let outDir  = makeAbsolute curDir outDir'

    cd repoDir

    mktree outDir

    let refJson = Text.pack outDir <> "/current.json"
    void $ shell ("yaml-includes -f " <> Text.pack repoDir <> "/" <> Text.pack metaDir <> " > " <> refJson) empty

    let cmd = Text.pack $ "git log --all --follow --pretty=format:\"%h\" -- " ++ metadataPath parsedOpts
    (n, hash) <- nl @Integer $ inshell cmd empty

    echo $ unsafeTextToLine $ ("Processing commit # " <> lineToText hash)
    tmpdir <- using (mktempdir "/tmp/" "clones")

    let cloneCmd = Text.pack $ "git clone . " ++ tmpdir
    echo $ unsafeTextToLine $ "Running: " <> cloneCmd
    void $ shell cloneCmd empty

    cd tmpdir

    void $ shell ("git checkout " <> lineToText hash) empty

    -- Now! We can run `yaml-includes`
    let prefix    = Text.pack $ Printf.printf "%010d" n
        localJson = Text.pack outDir <> "/" <> prefix <> "-" <> lineToText hash <> ".json"

    void $ shell ("yaml-includes -f " <> Text.pack metaDir <> " > " <> localJson) empty

    cd repoDir
