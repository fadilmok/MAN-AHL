module Test.ManAhl.Run (
  main
) where

import Control.Monad
import System.Exit (exitFailure)
import Text.Printf
import Test.QuickCheck

import qualified Test.ManAhl.Core.Analytics as Analytics
import qualified Test.ManAhl.Core.Random as Random

main :: IO()
main = do
  success <- forM (
      Analytics.tests ++
      Random.tests
    ) $ \ (s, t) -> printf "%-35s:" s >> t

  unless (all (==True) success)
    exitFailure

