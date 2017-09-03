-- | Testsuite for the Engine
module Test.ManAhl.Core.WeightedEngine(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.ManAhl.QuickCheck as Test

import ManAhl.Core.Types
import ManAhl.Core.Engine
import ManAhl.Core.Random
import ManAhl.Core.Analytics

import Control.Monad
import Data.Either
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Printf (printf)

-- | List of tests related to the engine
tests :: TestSuite
tests = [
    ("Engine distri check - Mersene", propWeightedProba Mersenne)
   ,("Engine distri check - Ecuyer",  propWeightedProba Ecuyer)
   ,("Engine fail",                   testEngineFail)
   ,("Engine Perf - Mersene",         propPerf Mersenne)
   ,("Engine Perf - Ecuyer",          propPerf Ecuyer)
  ]

-- | Test that we recover the input probabilities
-- using random pdf pillars as input
propWeightedProba :: UniformRNGType -> Test
propWeightedProba rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ forAll genPdfPillars $ \ pdfP ->
      let e = case mkEngineParams pdfP of
            Left s -> error s; Right x -> x
          stats = runStatEngine e rng $ allStats 1000000
          pdf' = unPDF $ pdf e
          proba = probaFromCount stats
       in Map.foldl (\ acc v -> if not acc then False else v <= 0.005 ) True $
            Map.mapWithKey (\ v p -> abs (proba ! Just v - p)) pdf'

-- | Test that EngineParams fails to build for all
-- expected cases
testEngineFail :: Test
testEngineFail = TestPure $ const $
      negativePro && nullPro && greaterPro && nullPro2
  where
      negativePro = isLeft $ mkEngineParams [(1, -1)]
      nullPro = isLeft $ mkEngineParams []
      nullPro2 = isLeft $ mkEngineParams [(1,0)]
      greaterPro = isLeft $ mkEngineParams [(1, 0.4), (2, 0.5), (3, 0.4)]

-- | Test that the performance of the Engine remain acceptable
propPerf :: UniformRNGType -> Test
propPerf rT =
  TestQCRng rT $ \ rng ->
    Test.runWith 10 $ forAll genPdfPillars $ \ pdfP -> monadicIO $
    do
      let e'= mkEngineParams pdfP
          e = case e' of Left s -> error s; Right x -> x
      t <- QC.run $ time $
          probaFromCount $
            runStatEngine e rng $ allStats 100000
      let res = t < 0.4
      unless res $
        QC.run $ printf "Time %s: %0.9f sec" (show rT) t
      assert res

