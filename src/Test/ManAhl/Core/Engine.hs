module Test.ManAhl.Core.Engine(
  tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC
import Test.ManAhl.QuickCheck as Test
import Test.ManAhl.Core.Analytics (genPdfPillars)

import ManAhl.Core.Types
import ManAhl.Core.Engine
import ManAhl.Core.Analytics

import Control.Monad
import Data.Either
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Printf (printf)

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

propWeightedProba :: UniformRNGType -> PdfPillars -> Property
propWeightedProba rT pdfP = monadicIO $
  do
    e' <- QC.run $ mkEngine pdfP $ Just rT
    let e = case e' of Left s -> error s; Right x -> x
        nexts = nextNums' e 1000000
        hist = mkHistogramWeighted nexts
        pdf' = unPDF $ pdf e
        stat = hsStat hist
        diff = Map.mapWithKey (\ v p -> abs (stat ! Just v - p)) pdf'
        res = Map.foldl (\ acc v -> if not acc then False else v <= 0.005 ) True diff
    unless res $
      QC.run $ print diff
    assert res

testEngineFail :: IO Bool
testEngineFail = do
  negativePro <- fmap isLeft $ mkEngine [(1, -1)] Nothing
  nullPro <- fmap isLeft $ mkEngine [] Nothing
  nullPro2 <- fmap isLeft $ mkEngine [(1,0)] Nothing
  greaterPro <- fmap isLeft $ mkEngine [(1, 0.4), (2, 0.5), (3, 0.4)] Nothing
  let res = negativePro && nullPro && greaterPro && nullPro2

  putStrLn $ "Test " ++ if res then "Passed" else "FAILED"
  return res

propPerf :: UniformRNGType -> PdfPillars -> Property
propPerf rT pdfP = monadicIO $
  do
    e' <- QC.run $ mkEngine pdfP $ Just rT
    let e = case e' of Left s -> error s; Right x -> x
    t <- QC.run $ time $ nextNums' e 100000

    let res = t < 0.35

    unless res $
      QC.run $ printf "Time %s: %0.9f sec" (show rT) t

    assert res

