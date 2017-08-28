module Test.ManAhl.Core.Random(
  tests
) where

import Test.QuickCheck
import Test.ManAhl.QuickCheck

import ManAhl.Core.Types
import ManAhl.Core.Random
import ManAhl.Core.Analytics

tests :: [(String, IO Bool)]
tests = [
    ("Ecuyer Mean and Std",   testRandomRNGMeanStd Ecuyer)
   ,("Mersenne Mean And Std", testRandomRNGMeanStd Mersenne)
   ,("Ecuyer Bounds",   testRandomRNGBounds Ecuyer)
   ,("Mersenne Bounds", testRandomRNGBounds Mersenne)
  ]

testRandomRNGMeanStd :: UniformRNGType -> IO Bool
testRandomRNGMeanStd rng = do
  rng <- mkUniformRNG $ Just rng
  let vals = nextVals rng nTests
      m = mean vals
      std = stdDev vals
      x ~= y = abs (x - y) <= 0.04
      res = m ~= 0.5 && std ~= 0.28
--  putStr $ "Mean: " ++ show m ++ " Std: " ++ show std ++ " ->"
  putStrLn $ " Test " ++ if res then "Passed" else "FAILED"
  return res

testRandomRNGBounds :: UniformRNGType -> IO Bool
testRandomRNGBounds rng = do
  rng <- mkUniformRNG $ Just rng
  let vals = nextVals rng nTests
      res = all (==True) $ map (\ x -> x >= 0 && x <= 1) vals
  putStrLn $ " Test " ++ if res then "Passed" else "FAILED"
  return res
