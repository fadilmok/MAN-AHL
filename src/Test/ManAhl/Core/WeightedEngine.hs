-- | Testsuite for the Engine
module Test.ManAhl.Core.WeightedEngine(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.ManAhl.QuickCheck as Test

import ManAhl.Core.Types
import ManAhl.Core.WeightedEngine
import ManAhl.Core.UniformEngine
import ManAhl.Core.Analytics

import Control.Monad
import Data.Either
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Printf (printf)

-- | List of tests related to the engine
tests :: TestSuite
tests = map (\(x, y) -> ("Weighted Engine - " ++ x, y))
  [
    ("distri check - Mersene", propWeightedProba Mersenne)
   ,("distri check - Ecuyer",  propWeightedProba Ecuyer)
   ,("fail",                   testEngineFail)
   ,("Perf - Mersene",         propPerf Mersenne)
   ,("Perf - Ecuyer",          propPerf Ecuyer)
  ]

-- | Test that we recover the input probabilities
-- using random pdf pillars as input
propWeightedProba :: UniformRNGType -> Test
propWeightedProba rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ forAll genPdfPillars $ \ pdfP ->
      let e = case mkWPEngineParams pdfP of
            Left s -> error s; Right x -> x
          stats = computeStats e rng
                    (allStats 1000000 :: StatWPEngine WeightedStats)
          pdf' = pdf e
          (Just proba) = hsProba stats
       in pdf' == fromPillars proba

-- | Test that EngineParams fails to build for all
-- expected cases
testEngineFail :: Test
testEngineFail = TestPure $ const $
      negativePro && nullPro && greaterPro && nullPro2
  where
      negativePro = isLeft $ mkWPEngineParams $ PdfPillars [(1, -1)]
      nullPro = isLeft $ mkWPEngineParams $ PdfPillars []
      nullPro2 = isLeft $ mkWPEngineParams $ PdfPillars [(1,0)]
      greaterPro = isLeft $ mkWPEngineParams $
                      PdfPillars [(1, 0.4), (2, 0.5), (3, 0.4)]

-- | Test that the performance of the Engine remain acceptable
propPerf :: UniformRNGType -> Test
propPerf rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ forAll genPdfPillars $ \ pdfP -> monadicIO $
    do
      let e'= mkWPEngineParams pdfP
          e = case e' of Left s -> error s; Right x -> x
      t <- QC.run $ time $
              computeStats e rng
                (allStats 100000 :: StatWPEngine WeightedStats)
      let res = t < 0.4
      unless res $
        QC.run $ printf "Time %s: %0.9f sec" (show rT) t
      assert res

