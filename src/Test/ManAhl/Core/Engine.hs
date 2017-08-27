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
      Test.run $ forAll genPdfPillars propMkEngine)
  ]

propMkEngine :: PdfPillars -> Property
propMkEngine pdfP = monadicIO $
  do
    e <- QC.run $ mkEngine pdfP $ Just Mersenne
    let nexts = nextNums' e 10000
        hist = mkHistogram nexts
        pdf = unPDF $ mkPdf pdfP
        res = Map.fromList $ hsStat hist
    assert $ all (==True) $
      map (\(v, p) -> abs (res ! Just v - p) < 1e-2) pdf

