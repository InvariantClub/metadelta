module Main where

import "optparse-generic" Options.Generic

import Hasa.App ( loadMetadata, run )

main :: IO ()
main = unwrapRecord "hasura-to-icf" >>= run loadMetadata
