-- | Testsuite for the Engine
module Test.ManAhl.Core.Engine(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.ManAhl.QuickCheck as Test

import ManAhl.Core.Types
import ManAhl.Core.Engine
import ManAhl.Core.Analytics

import Control.Monad
import Data.Either
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Printf (printf)

-- | List of tests related to the engine
tests :: [(String, IO Bool)]
tests = [
    ("Engine distri check - Mersene",
        Test.runWith 10 $ forAll genPdfPillars $ propWeightedProba Mersenne)
   ,("Engine distri check - Ecuyer",
        Test.runWith 10 $ forAll genPdfPillars $ propWeightedProba Ecuyer)
   ,("Engine fail",
        testEngineFail)
   ,("Engine Perf - Mersene",
        Test.runWith 10 $ forAll genPdfPillars $ propPerf Mersenne)
   ,("Engine Perf - Ecuyer",
        Test.runWith 10 $ forAll genPdfPillars $ propPerf Ecuyer)
  ]

-- | Test that we recover the input probabilities
-- using random pdf pillars as input
propWeightedProba :: UniformRNGType -> PdfPillars -> Property
propWeightedProba rT pdfP = monadicIO $
  do
    let e' = mkEngineParams pdfP $ Just rT
    let e = case e' of Left s -> error s; Right x -> x
    stats <- QC.run $ runStatEngine e $ allStats 1000000
    let pdf' = unPDF $ pdf e
        proba = probaFromCount stats
        diff = Map.mapWithKey (\ v p -> abs (proba ! Just v - p)) pdf'
        res = Map.foldl (\ acc v -> if not acc then False else v <= 0.005 ) True diff
    unless res $
      QC.run $ print diff
    assert res

-- | Test that EngineParams fails to build for all
-- expected cases
testEngineFail :: IO Bool
testEngineFail = do
  let negativePro = isLeft $ mkEngineParams [(1, -1)] Nothing
      nullPro = isLeft $ mkEngineParams [] Nothing
      nullPro2 = isLeft $ mkEngineParams [(1,0)] Nothing
      greaterPro = isLeft $ mkEngineParams [(1, 0.4), (2, 0.5), (3, 0.4)] Nothing
      res = negativePro && nullPro && greaterPro && nullPro2

  putStrLn $ "Test " ++ if res then "Passed" else "FAILED"
  return res

-- | Test that the performance of the Engine remain acceptable
propPerf :: UniformRNGType -> PdfPillars -> Property
propPerf rT pdfP = monadicIO $
  do
    let e'= mkEngineParams pdfP $ Just rT
    let e = case e' of Left s -> error s; Right x -> x
    t <- QC.run $ time $
      fmap probaFromCount $
        runStatEngine e $ allStats 100000

    let res = t < 0.4

    unless res $
      QC.run $ printf "Time %s: %0.9f sec" (show rT) t

    assert res

