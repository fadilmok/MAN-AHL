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
    ("Mersenne Mean And Std", testRandomRNG Mersenne)
   ,("Ecuyer Mean and Std",   testRandomRNG Ecuyer)
  ]


testRandomRNG :: UniformRNGType -> IO Bool
testRandomRNG rng = do
  rng <- mkUniformRNG $ Just rng
  let vals = nextVals rng 1000
      m = mean vals
      std = stdDev vals
      x ~= y = abs (x - y) <= 0.04
      res = m ~= 0.5 && std ~= 0.28
  putStrLn $ "Test " ++ if res then "Passed" else "Failed"
  return res
