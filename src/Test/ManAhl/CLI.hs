-- | Module testing the Command line input
module Test.ManAhl.CLI(
  tests
) where

import ManAhl.CLI
import ManAhl.Core.Types

import Control.Monad

-- | Testsuite
tests :: [(String, IO Bool)]
tests = [
    ("CLI Query", testQuery)
   ,("CLI Run",            testRun)
  ]

-- | Test the creation of a query from the arguments
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

-- | Test a run for a given query
testRun :: IO Bool
testRun = do
  let q1 = RunWeightedWith [(1, 0.5), (2, 0.5)] 1000000 Mersenne
      q2 = RunUniformWith 1000000 Mersenne

  r1' <- run q1
  let r1 = either (const False) ( \ (ResultWeighted _ r) ->
          foldl (\ acc (_, x) ->
            if not acc then False
              else round (x * 100) == 50) True r
          ) r1'

  r2' <- run q2
  let r2 = either (const False) (\ (ResultUniform _ r) ->
          foldl (\ acc (_, x) ->
            if not acc then False
              else round (x * 100) ==
                round (100 / fromIntegral (length r))) True r
          ) r2'
  let res = r1 && r2
  unless res $ do
    print $ "RunWeighted: " ++ show r1'
    print $ "RunUniform: " ++ show r2'

  putStrLn $ "Test " ++ if res then "Passed" else "FAILED"
  return res
