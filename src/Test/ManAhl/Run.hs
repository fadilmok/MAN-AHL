{-# OPTIONS_GHC -O2 #-}
-- | Module to run all the testsuites
module Test.ManAhl.Run (
  main
) where

import Control.Monad
import System.Exit (exitFailure)
import Text.Printf
import Test.QuickCheck

import ManAhl.Core.Random
import Test.ManAhl.QuickCheck (runTest)
import qualified Test.ManAhl.Core.Analytics as Analytics
import qualified Test.ManAhl.Core.UniformEngine as Uniform
import qualified Test.ManAhl.Core.WeightedEngine as WeightedEngine
import qualified Test.ManAhl.CLI as CLI

-- | Main to run the testsuite, does not stop for failure
main :: IO()
main = do
  success <- forM (
      CLI.tests ++
      Analytics.tests ++
      UniformEngine.tests ++
      WeightedEngine.tests
   ) $ \ (name, test) -> do
           printf "%-35s:" name
           runTest test

  unless (all (==True) success)
    exitFailure

