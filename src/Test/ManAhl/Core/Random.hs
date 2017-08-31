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

tests :: [(String, IO Bool)]
tests = [
    ("Ecuyer Mean and Std",   Test.runWith 100 $ propMeanStd Ecuyer)
   ,("Mersenne Mean And Std", Test.runWith 100 $ propMeanStd Mersenne)
   ,("Ecuyer Bounds",         Test.runWith 100 $ propBounds Ecuyer)
   ,("Mersenne Bounds",       Test.runWith 100 $ propBounds Mersenne)
   ,("Mersenne Uniform",      Test.runWith 10 $ propUniform Mersenne)
   ,("Ecuyer Uniform",        Test.runWith 10 $ propUniform Ecuyer)
   ,("Perf Comp RNGs",        Test.runWith 10 propPerf)
  ]

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

propBounds :: UniformRNGType -> Property
propBounds rng = monadicIO $ do
  vals <- QC.run $ runProbaUni (Just rng) $ nextVals nRand
  assert $
    foldl (\ acc x -> if not acc then False else  x >= 0 && x <= 1) True vals

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

propPerf :: Property
propPerf = monadicIO $
  do
    tE <- QC.run $ time $
      fmap probaFromCount $
        runStatUni (Just Ecuyer) $ allUStats 100000

    tM <- QC.run $ time $
      fmap probaFromCount $
        runStatUni (Just Mersenne) $ allUStats 100000

    let res = tE < 0.4 && tM < 0.4

    unless res $ do
      QC.run $ printf "Time Mersenne: %0.9f sec" tM
      QC.run $ printf "Time Ecuyer: %0.9f sec" tE

    assert res
