-- | Testsuite for the Uniform Random Engine
module Test.ManAhl.Core.Random(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.ManAhl.QuickCheck as Test
import Control.Monad
import qualified Data.Map as Map
import Text.Printf (printf)

import ManAhl.Core.Types
import ManAhl.Core.Random
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
    Test.runWith 100 $
      let vals = runProbaUni rng $ nextVals nRand
          m = mean vals
          std = stdDev vals
          x ~= y = abs (x - y) <= 0.04
       in m ~= 0.5 && std ~= 0.28

-- | Ensure that the uniform engine generate probilities within
-- the expected bounds
propBounds :: UniformRNGType -> Test
propBounds rngT =
  TestQCRng rngT $ \ rng ->
    Test.runWith 10 $
      foldl (\ acc x -> if not acc then False else  x >= 0 && x <= 1) True $
        runProbaUni rng $ nextVals nRand

-- | Test the distribution of the uniform engine is uniform
propUniform :: UniformRNGType -> Test
propUniform rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $
      let stats = runStatUni rng $ allUStats nRand in
      Map.foldl (\ acc x ->
        if not acc then False
              else round (x * 100) ==
                round (100 / fromIntegral (Map.size $ hsCount stats))) True $
        probaFromCount stats

-- | Ensure that the performance of the uniform engine
-- remain acceptable
propPerf :: UniformRNGType -> Test
propPerf rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ monadicIO $
  do
    t <- QC.run $ time $
        probaFromCount $
          runStatUni rng $ allUStats 100000
    let res = t < 0.4
    unless res $ do
      QC.run $ printf "Time %s: %0.9f sec" (show rT) t
    assert res
