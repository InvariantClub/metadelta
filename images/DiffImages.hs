#!/usr/bin/env runhaskell

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = do
  mainWith d >> putStrLn "Done!"

h = sRGB24read

ad :: Diagram B
ad = rect 5 100
    -- For the light one:
    # fcA (withOpacity (h "#99dd9a") 0.20)
    -- For the normal one:
    -- # fc (h "#99dd9a")
    # lw none

added = (hsep 5 (replicate 10 ad) ||| phantom (ad) )
    # rotate ((-45) @@ deg)
    # centerXY
    # clipTo (square (100 / sqrt 2))

r' :: Diagram B
r' = rect 5 100
    -- For the light one:
    # fcA (withOpacity lightRed 0.20)
    -- For the normal one:
    -- # fc lightRed
    # lw none

darkRed = h "#dd6e6c"
lightRed = h "#dd9a99"


removed = (hsep 5 (replicate 10 r') ||| phantom r')
    # rotate (45 @@ deg)
    # centerXY
    # clipTo (square (100 / sqrt 2))

d = removed
-- d = added
