module Test.ManAhl.Core.Random(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.ManAhl.QuickCheck as Test
import Control.Exception (evaluate)
import Control.DeepSeq
import Control.Monad
import qualified Data.Map as Map
import System.CPUTime
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
    let vals = nextVals rng nRand
        stats = mkHistogramUniform vals
    let res =
          Map.foldl (\ acc x ->
            if not acc then False
                    else round (x * 100) ==
                         round (100 / fromIntegral (Map.size $ hsCount stats))) True $
              hsStat stats
    unless res $ do
      QC.run $ print stats
    assert res

propPerf :: Property
propPerf = monadicIO $
  do
    let time :: NFData t => t -> IO Double
        time f = do
          start <- getCPUTime
          x <- evaluate f
          rnf x `seq` return()
          end <- getCPUTime
          return $ fromIntegral (end - start) / 10^12


    rngE <- QC.run $ mkUniformRNG $ Just Ecuyer
    tE <- QC.run $ time $ nextVals rngE 100000

    rngM <- QC.run $ mkUniformRNG $ Just Mersenne
    tM <- QC.run $ time $ nextVals rngM 100000

    let res = tE < 0.3 && tM < 0.3

    unless res $ do
      QC.run $ printf "Time Mersenne: %0.9f sec" tM
      QC.run $ printf "Time Ecuyer: %0.9f sec" tE

    assert res
