module Test.ManAhl.CLI(
  tests
) where

import ManAhl.CLI
import ManAhl.Core.Types

tests :: [(String, IO Bool)]
tests = [
    ("CLI Query creation", testQuery)
  ]

testQuery :: IO Bool
testQuery = do
  let t1 = parse [] == Just (RunWeightedWith pillarsDefault 1000000 Mersenne)
      t2 = parse ["-rng=Ecuyer"] ==
                Just (RunWeightedWith pillarsDefault 1000000 Ecuyer)
      t3 = parse ["-rng=Ecuyer","-nSims=1"] ==
                Just (RunWeightedWith pillarsDefault 1 Ecuyer)
      t4 = parse ["-rng=Ecuyer","-nSims=1","-pillars=[(1,0),(2,1)]"] ==
                Just (RunWeightedWith [(1, 0), (2, 1)] 1 Ecuyer)
      t5 = parse ["-rng=Ecuyer","-nSims=1","-run=Uniform"] ==
                Just (RunUniformWith 1 Ecuyer)
      res = t1 && t2 && t3 && t4 && t5

  putStrLn $ "Test " ++ if res then "Passed" else "FAILED"
  return res
