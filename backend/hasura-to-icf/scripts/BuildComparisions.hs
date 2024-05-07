#!/usr/bin/env runhaskell

module Main where

import Control.Monad ( forM )
import Data.List ( isSuffixOf, sort )
import Data.Maybe ( catMaybes )
import System.Directory ( getDirectoryContents )
import System.Environment ( getArgs )
import System.Exit ( ExitCode (..) )
import System.Process ( system )

main :: IO ()
main = do
  [path] <- getArgs
  files  <- id <$> sort <$> filter ("json" `isSuffixOf`) <$> getDirectoryContents path

  -- let pairs = compositions 2 files
  let pairs = zipWith (\a b -> [a, b]) files (drop 1 files)

  validPairs <- forM pairs $ \p@[new, old] -> do
    let new' = path ++ "/" ++ new
        old' = path ++ "/" ++ old
    c <- system ("cabal exec hasura-to-icf -- diff -n " ++ new' ++ " -o " ++ old' ++ ">/dev/null 2>&1")
    case c of
      ExitFailure 1 -> pure Nothing
      ExitSuccess   -> pure $ Just p
      -- For `jd`
      -- ExitSuccess   -> pure Nothing
      -- ExitFailure 1 -> pure $ Just p

  let pairs' = catMaybes validPairs
      cmds   = map genCliCommand pairs'

  putStrLn $ unlines cmds


genCliCommand :: [String] -> String
genCliCommand [new, old] =
  "cabal exec hasura-to-icf -- diff -n " ++ new ++ " -o " ++ old ++ ">out/" ++ old ++ "-to-" ++ new


putCurrentAtStart :: [a] -> [a]
putCurrentAtStart xs = take (length xs) $ take 1 (reverse xs) ++ xs


compositions :: Int -> [a] -> [[a]]
compositions k xs
    | k > length xs = []
    | k <= 0        = [[]]
    | otherwise     = csWithoutHead ++ csWithHead
    where csWithoutHead = compositions k $ tail xs
          csWithHead    = [ head xs : ys | ys <- compositions (k - 1) $ tail xs ]
