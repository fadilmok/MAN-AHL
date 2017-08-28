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

import qualified Data.Map as Map
import Data.Map ((!))

tests :: [(String, IO Bool)]
tests = [
    ("Engine test - Mersene",
        Test.run $ forAll genPdfPillars $ propMkEngine Mersenne)
   ,("Engine test - Ecuyer",
        Test.run $ forAll genPdfPillars $ propMkEngine Ecuyer)
  ]

propMkEngine :: UniformRNGType -> PdfPillars -> Property
propMkEngine rT pdfP = monadicIO $
  do
    e <- QC.run $ mkEngine pdfP $ Just rT
    let nexts = nextNums' e nTests
        hist = mkHistogram nexts
        pdf = Map.toList $ unPDF $ mkPdf pdfP
        res = Map.fromList $ hsStat hist
    assert $ all (==True) $
      map (\(v, p) -> abs (res ! Just v - p) < 0.04) pdf

