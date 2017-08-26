module Test.ManAhl.Run (
  main
) where

import Control.Monad
import Text.Printf
import Test.QuickCheck

import Test.ManAhl.Core.Analytics

main :: IO()
main = do
  forM_ tests $
    \ (s, t) -> printf "%-25s:" s >> t


