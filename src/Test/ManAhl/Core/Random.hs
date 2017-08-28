module Test.ManAhl.Core.Random(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.ManAhl.QuickCheck as Test
import Control.Monad
import qualified Data.Map as Map

import ManAhl.Core.Types
import ManAhl.Core.Random
import ManAhl.Core.Analytics

tests :: [(String, IO Bool)]
tests = [
    ("Ecuyer Mean and Std",   Test.runWith 100 $ propMeanStd Ecuyer)
   ,("Mersenne Mean And Std", Test.runWith 100 $ propMeanStd Mersenne)
   ,("Ecuyer Bounds",         Test.runWith 100 $ propBounds Ecuyer)
   ,("Mersenne Bounds",       Test.runWith 100 $ propBounds Mersenne)
   ,("Mersenne Uniform",      Test.runWith 10 $ propUniform Mersenne)
   ,("Ecuyer Uniform",        Test.runWith 10 $ propUniform Ecuyer)
  ]

propMeanStd :: UniformRNGType -> Property
propMeanStd rng = monadicIO $ do
  rng <- QC.run $ mkUniformRNG $ Just rng
  let vals = nextVals rng nRand
      m = mean vals
      std = stdDev vals
      x ~= y = abs (x - y) <= 0.04
      res = m ~= 0.5 && std ~= 0.28
  unless res $
    QC.run $ putStrLn $ "Mean: " ++ show m ++ " Std: " ++ show std ++ " ->"
  assert res

propBounds :: UniformRNGType -> Property
propBounds rng = monadicIO $ do
  rng <- QC.run $ mkUniformRNG $ Just rng
  let vals = nextVals rng nRand
      res = foldl (\ acc x -> if not acc then False else  x >= 0 && x <= 1) True vals
  assert res

propUniform :: UniformRNGType -> Property
propUniform rT = monadicIO $
  do
    rng <- QC.run $ mkUniformRNG $ Just rT
    let ranges :: Map.Map Double Int
        ranges = Map.fromList $ zip (map (/100)[1,2 .. 100]) [0..]
        n = nRand
        vals = nextVals rng n
        count = foldl (\ m x -> Map.insertWith (+)
          (case x `Map.lookupGE` ranges of Just y -> fst y; Nothing -> error "Failure") 1 m)
            ranges vals
        totalCount = Map.foldl (\acc x -> x + acc) 0 count
        proba = Map.map (\ x -> round $ fromIntegral x * 100 / fromIntegral n) count
    let res =
          foldl (\ acc (_, x) ->
            if not acc then False else x == round (100 / fromIntegral (Map.size ranges))) True $
              Map.toList proba
    unless res $ do
      QC.run $ print totalCount
      QC.run $ print count
      QC.run $ print proba
    assert res
