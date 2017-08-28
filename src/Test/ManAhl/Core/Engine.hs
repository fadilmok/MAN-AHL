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
import qualified Data.Map as Map
import Data.Map ((!))

tests :: [(String, IO Bool)]
tests = [
    ("Engine test - Mersene",
        Test.runWith 10 $ forAll genPdfPillars $ propWeightedProba Mersenne)
   ,("Engine test - Ecuyer",
        Test.runWith 10 $ forAll genPdfPillars $ propWeightedProba Ecuyer)
  ]

propWeightedProba :: UniformRNGType -> PdfPillars -> Property
propWeightedProba rT pdfP = monadicIO $
  do
    e' <- QC.run $ mkEngine pdfP $ Just rT
    let e = case e' of Left s -> error s; Right x -> x
        nexts = nextNums' e $ 1000000
        hist = mkHistogram nexts
        pdf' = unPDF $ pdf e
        stat = Map.fromList $ hsStat hist
        diff = Map.mapWithKey (\ v p -> abs (stat ! Just v - p)) pdf'
        res = Map.foldl (\ acc v -> if not acc then False else v <= 0.005 ) True diff
    unless res $
      QC.run $ print diff
    assert res
