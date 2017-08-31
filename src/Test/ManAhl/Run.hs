-- | Module to run all the testsuites
module Test.ManAhl.Run (
  main
) where

import Control.Monad
import System.Exit (exitFailure)
import Text.Printf
import Test.QuickCheck

import qualified Test.ManAhl.Core.Analytics as Analytics
import qualified Test.ManAhl.Core.Random as Random
import qualified Test.ManAhl.Core.Engine as Engine
import qualified Test.ManAhl.CLI as CLI

-- | Main to run the testsuite, does not stop for failure
main :: IO()
main = do
  success <- forM (
      CLI.tests ++
      Analytics.tests ++
      Random.tests ++
      Engine.tests
    ) $ \ (s, t) -> printf "%-35s:" s >> t

  unless (all (==True) success)
    exitFailure

