module Test.ManAhl.Run (
  main
) where

import Control.Monad
import System.Exit (exitFailure)
import Text.Printf
import Test.QuickCheck

import qualified Test.ManAhl.Core.Analytics as Analytics

main :: IO()
main = do
  success <- forM (
      Analytics.tests
    ) $ \ (s, t) -> printf "%-35s:" s >> t

  unless (all (== True) success)
    exitFailure


