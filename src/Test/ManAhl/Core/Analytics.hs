{-# LANGUAGE ScopedTypeVariables #-}
-- | Module containing the Analytics unit tests
module Test.ManAhl.Core.Analytics(
  tests
) where

import ManAhl.Core.Analytics
import ManAhl.Core.Types

import Control.Exception
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck
import Test.ManAhl.QuickCheck

-- Instance to create correct PDFs
instance Arbitrary PDF where
  arbitrary = do
      pillars <- genPdfPillars
      let pdf = mkPdf pillars
      return $ case pdf of
        Left s -> error $ "Generation failed " ++ s
        Right p -> p

-- Instance to create correct CDFs
instance Arbitrary CDF where
  arbitrary = mkCdf `liftM` arbitrary

-- Instance to create correct PDFs
instance Arbitrary InvCDF where
  arbitrary = mkInvCdf `liftM` arbitrary

-- Generatoring input for the inverseCDF testing
genInvCdfQuery :: Gen (PDF, [Double])
genInvCdfQuery = do
  pdf <- arbitrary
  n <- choose (1, nTests)
  xs <- replicateM n $ choose(0, 1)
  return (pdf, xs)

-- Testsuite
tests :: TestSuite
tests = map (\(x, y) -> ("Analytics - " ++ x, y))
  [
    ("InvCDF Creation",                 propInvCdfFromPdfComplete)
   ,("CDF and PDF pillars check",       propCdfPdfPillars)
   ,("PDF stable with reverse Pillars", propPdfStable)
   ,("PDF consistency",                 propPdfConsistency)
   ,("InvCDF valid",                    propInvCdfValid)
   ,("PDF Failure",                     testPdfFail)
   ,("Inverse CDF Failure",             testInvCdfFail)
   ,("Mean Failure",                    testMeanFail)
   ,("Std Failure",                     testStdFail)
   ,("PDF No regression",               testPDFNoRegression)
   ,("InvCDF No regression",            testInvCDFNoRegression)
  ]

-- | Ensure that the last pillars of the CDF is 100%
propInvCdfFromPdfComplete :: Test
propInvCdfFromPdfComplete =
  TestQC $ run $ \pdf ->
    let iCdf = mkInvCdf $ mkCdf pdf
        lastP = fst $ last $ toPillars iCdf
     in abs ( lastP - 1) <= 0.0001

-- | Ensure that the PDF and CDF have the correct number of pillars
propCdfPdfPillars :: Test
propCdfPdfPillars =
  TestQC $ run $ \pdf ->
    let cdfPillars = fst $ unzip $ toPillars $ mkCdf pdf
        pdfPillars = fst $ unzip $ toPillars pdf
        invCdfPillars = snd $ unzip $ toPillars $ mkInvCdf $ mkCdf pdf
     in (length cdfPillars == length pdfPillars &&
            length pdfPillars == length invCdfPillars)
        &&
        (all (`elem` cdfPillars) pdfPillars &&
          all (`elem` pdfPillars) cdfPillars &&
            all (`elem` pdfPillars) invCdfPillars &&
              all (`elem` invCdfPillars) pdfPillars)

-- | Ensure that the pdf construction is stable
propPdfStable :: Test
propPdfStable =
  TestQC $ run $ forAll genPdfPillars $ \ (PdfPillars pdfP) ->
    mkPdf (PdfPillars pdfP) == mkPdf ( PdfPillars $ reverse $ pdfP )

-- | Ensure that the pdf probabilities are correct.
propPdfConsistency :: Test
propPdfConsistency =
  TestQC $ run $ \ (pdf :: PDF) ->
    let s = cFoldl (\acc x -> x + acc) 0 pdf
     in abs (s - 1) <= 0.0001

-- | Ensure that the inverseCdf recovers the correct pdf pillars
propInvCdfValid :: Test
propInvCdfValid =
  TestQC $ run $ forAll genInvCdfQuery $ \(pdf, xs) ->
    let cdf = mkCdf pdf
        iCdf = mkInvCdf cdf
        set = Set.fromList $ fst $ unzip $ toPillars pdf
     in all (==True) $
           map (\ x -> invCdf iCdf x `Set.member` set) xs

-- | Ensure that the PDF construction fails when expected
testPdfFail :: Test
testPdfFail =
  TestPure $ const $
      negativePro && nullPro && greaterPro && nullPro2
  where
      negativePro = isLeft $ mkPdf $ PdfPillars [(1, -1)]
      nullPro = isLeft $ mkPdf $ PdfPillars []
      nullPro2 = isLeft $ mkPdf $ PdfPillars [(1,0)]
      greaterPro = isLeft $ mkPdf $ PdfPillars [(1, 0.4), (2, 0.5), (3, 0.4)]

-- | Ensure that inverse CDF fails when expected
testInvCdfFail :: Test
testInvCdfFail =
  TestIO $ failTest $
    let iCdf = fromPillars [(0.5, Just 1), (0.8, Just 2), (1, Just 3)]
    in invCdf iCdf 1.1

-- | Ensure that the mean fails when expected
testMeanFail :: Test
testMeanFail =
  TestIO $ failTest $ mean []

-- | Ensure that the stdDev fails when expected
testStdFail :: Test
testStdFail =
  TestIO $ failTest $ stdDev []

-- | Non regression test for the pdf
testPDFNoRegression :: Test
testPDFNoRegression =
  TestPure $ const $ test
    where
      pdfP = PdfPillars [(1, 0.2), (1, 0.1), (2, 0.4), (3, 0.3),(4, 0)]
      res  = mkPdf pdfP
      test = case res of
              Left _ -> False
              Right p ->
                p == (fromPillars [(Just 1, 0.3), (Just 2, 0.4), (Just 3, 0.3)])

-- | Non regression test for the cdf
testInvCDFNoRegression :: Test
testInvCDFNoRegression =
  TestPure $ const test
    where
      pdfP = PdfPillars [(1, 0.2), (1, 0.1), (2, 0.4), (3, 0.3),(4, 0)]
      res  = mkPdf pdfP
      test = case res of
              Left _ -> False
              Right pdf ->
                mkInvCdf (mkCdf pdf) ==
                  fromPillars [(0.3, Just 1), (0.7, Just 2), (1, Just 3)]

