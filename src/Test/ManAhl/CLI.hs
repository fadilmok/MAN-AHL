-- | Module testing the Command line input
module Test.ManAhl.CLI(
  tests
) where

import ManAhl.CLI
import ManAhl.Core.Types
import Test.ManAhl.QuickCheck hiding (run)

import Control.Monad

-- | Testsuite
tests :: TestSuite
tests = [
    ("CLI Query", testQuery)
   ,("CLI Run",   testRun)
  ]

-- | Test the creation of a query from the arguments
testQuery :: Test
testQuery = TestPure $ const $ t1 && t2 && t3 && t4 && t5
  where
      t1 = parse [] == Just (RunWeightedWith pillarsDefault 1000000 Mersenne)
      t2 = parse ["-rng=Ecuyer"] ==
                Just (RunWeightedWith pillarsDefault 1000000 Ecuyer)
      t3 = parse ["-rng=Ecuyer","-nSims=1"] ==
                Just (RunWeightedWith pillarsDefault 1 Ecuyer)
      t4 = parse ["-rng=Ecuyer","-nSims=1","-pillars=[(1,0),(2,1)]"] ==
                Just (RunWeightedWith (PdfPillars [(1, 0), (2, 1)]) 1 Ecuyer)
      t5 = parse ["-rng=Ecuyer","-nSims=1","-run=Uniform"] ==
                Just (RunUniformWith 1 Ecuyer)

-- | Test a run for a given query
testRun :: Test
testRun = TestIO $ do
  let q1 = RunWeightedWith (PdfPillars [(1, 0.5), (2, 0.5)]) 1000000 Mersenne
      q2 = RunUniformWith 1000000 Mersenne

  r1' <- run q1
  let r1 = either (const False) ( \ (ResultWeighted (Stats _ _ (Just r))) ->
          foldl (\ acc (_, x) ->
            if not acc then False
              else round (x * 100) == 50) True r
          ) r1'

  r2' <- run q2
  let r2 = either (const False) (\ (ResultUniform (Stats _  _ (Just r))) ->
          foldl (\ acc (_, x) ->
            if not acc then False
              else round (x * 100) ==
                round (100 / fromIntegral (length r))) True r
          ) r2'
  let res = r1 && r2
  unless res $ do
    print $ "RunWeighted: " ++ show r1'
    print $ "RunUniform: " ++ show r2'

  return res
