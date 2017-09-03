{-# LANGUAGE ScopedTypeVariables #-}
-- | Testsuite for the Uniform Random Engine
module Test.ManAhl.Core.UniformEngine(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.ManAhl.QuickCheck as Test

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf (printf)

import ManAhl.Core.Types
import ManAhl.Core.UniformEngine
import ManAhl.Core.Analytics

-- | List of tests
tests :: TestSuite
tests = [
    ("Ecuyer Mean and Std",   propMeanStd Ecuyer)
   ,("Mersenne Mean And Std", propMeanStd Mersenne)
   ,("Ecuyer Bounds",         propBounds Ecuyer)
   ,("Mersenne Bounds",       propBounds Mersenne)
   ,("Mersenne Uniform",      propUniform Mersenne)
   ,("Ecuyer Uniform",        propUniform Ecuyer)
   ,("Uniform Mersenee Perf", propPerf Mersenne)
   ,("Uniform Ecuyer Perf",   propPerf Ecuyer)
  ]

-- | Test that the standard deviation and mean of the uniform
-- engine is constant.
propMeanStd :: UniformRNGType -> Test
propMeanStd rngT =
  TestQCRng rngT $ \ rng ->
    Test.runWith 10 $ \ (x :: Int) ->
      let vals = computeProba Nothing rng (nextNums nRand :: ProbaUniEngine [Double])
          m = mean vals
          std = stdDev vals
          x ~= y = abs (x - y) <= 0.04
       in m ~= 0.5 && std ~= 0.28

-- | Ensure that the uniform engine generate probilities within
-- the expected bounds
propBounds :: UniformRNGType -> Test
propBounds rngT =
  TestQCRng rngT $ \ rng ->
    Test.runWith 10 $ \ (x :: Int) ->
      foldl (\ acc x -> if not acc then False else  x >= 0 && x <= 1) True $
        hsDistriUni $
          computeProba Nothing rng (nextNums nRand :: ProbaUniEngine [Double])

-- | Test the distribution of the uniform engine is uniform
propUniform :: UniformRNGType -> Test
propUniform rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ \ (x :: Int) ->
      let stats = computeStats Nothing rng (allStats nRand :: StatUniEngine Stats)
       in Map.foldl (\ acc x ->
          if not acc then False
              else round (x * 100) ==
                round (100 / fromIntegral (Map.size $ hsDistri stats))) True $
                  fromJust $ hsProbaUni stats

-- | Ensure that the performance of the uniform engine
-- remain acceptable
propPerf :: UniformRNGType -> Test
propPerf rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ monadicIO $
  do
    t <- QC.run $ time $
            computeStats Nothing rng (allStats 1000000 :: StatUniEngine Stats)

    let res = t < 0.4
    unless res $ do
      QC.run $ printf "Time %s: %0.9f sec" (show rT) t
    assert res
