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
tests :: [(String, IO Bool)]
tests = [
    ("Ecuyer Mean and Std",   Test.runWith 100 $ propMeanStd Ecuyer)
   ,("Mersenne Mean And Std", Test.runWith 100 $ propMeanStd Mersenne)
   ,("Ecuyer Bounds",         Test.runWith 100 $ propBounds Ecuyer)
   ,("Mersenne Bounds",       Test.runWith 100 $ propBounds Mersenne)
   ,("Mersenne Uniform",      Test.runWith 10 $ propUniform Mersenne)
   ,("Ecuyer Uniform",        Test.runWith 10 $ propUniform Ecuyer)
   ,("Uniform Mersenee Perf", Test.runWith 10 $ propPerf Mersenne)
   ,("Uniform Ecuyer Perf",   Test.runWith 10 $ propPerf Ecuyer)
  ]

-- | Test that the standard deviation and mean of the uniform
-- engine is constant.
propMeanStd :: UniformRNGType -> Property
propMeanStd rng = monadicIO $ do
  vals <- QC.run $ runProbaUni (Just rng) $ nextVals nRand
  let m = mean vals
      std = stdDev vals
      x ~= y = abs (x - y) <= 0.04
      res = m ~= 0.5 && std ~= 0.28
  unless res $
    QC.run $ putStrLn $ "Mean: " ++ show m ++ " Std: " ++ show std ++ " ->"
  assert res

-- | Ensure that the uniform engine generate probilities within
-- the expected bounds
propBounds :: UniformRNGType -> Property
propBounds rng = monadicIO $ do
  vals <- QC.run $ runProbaUni (Just rng) $ nextVals nRand
  assert $
    foldl (\ acc x -> if not acc then False else  x >= 0 && x <= 1) True vals

-- | Test the distribution of the uniform engine is uniform
propUniform :: UniformRNGType -> Property
propUniform rT = monadicIO $
  do
    stats <- QC.run $ runStatUni (Just rT) $ allUStats nRand
    let res =
          Map.foldl (\ acc x ->
            if not acc then False
              else round (x * 100) ==
                round (100 / fromIntegral (Map.size $ hsCount stats))) True $
             probaFromCount stats
    unless res $ do
      QC.run $ print stats
    assert res

-- | Ensure that the performance of the uniform engine
-- remain acceptable
propPerf :: UniformRNGType -> Property
propPerf rT = monadicIO $
  do
    t <- QC.run $ time $
      fmap probaFromCount $
        runStatUni (Just rT) $ allUStats 100000

    let res = t < 0.4

    unless res $ do
      QC.run $ printf "Time %s: %0.9f sec" (show rT) t

    assert res
