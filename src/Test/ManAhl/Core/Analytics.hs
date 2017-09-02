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

-- Generatoring input for the inverseCDF testing
genInvCdfQuery :: Gen (PDF, [Double])
genInvCdfQuery = do
  pdf <- arbitrary
  n <- choose (1, nTests)
  xs <- replicateM n $ choose(0, 1)
  return (pdf, xs)

-- Testsuite
tests :: TestSuite
tests = [
    ("CDF Creation",                    propCdfFromPdfComplete)
   ,("CDF and PDF pillars check",       propCdfPdfPillars)
   ,("PDF stable with reverse Pillars", propPdfStable)
   ,("PDF consistency",                 propPdfConsistency)
   ,("InvCDF valid",                    propInvCdfValid)
   ,("PDF Failure",                     testPdfFail)
   ,("CDF Failure",                     testCdfFail)
   ,("Inverse CDF Failure",             testInvCdfFail)
   ,("Mean Failure",                    testMeanFail)
   ,("Std Failure",                     testStdFail)
   ,("PDF No regression",               testPDFNoRegression)
   ,("CDF No regression",               testCDFNoRegression)
  ]

-- | Ensure that the last pillars of the CDF is 100%
propCdfFromPdfComplete :: Test
propCdfFromPdfComplete =
  TestQC $ run $ \pdf ->
    let cdf = mkCdf pdf
        pillarsCdf = Map.toList $ unCDF cdf
     in fst (last pillarsCdf) == 1

-- | Ensure that the PDF and CDF have the correct number of pillars
propCdfPdfPillars :: Test
propCdfPdfPillars =
  TestQC $ run $ \pdf ->
    let cdfPillars = catMaybes $ Map.elems $ unCDF $ mkCdf pdf
        pdfPillars = Map.keys $ unPDF pdf
     in (length cdfPillars == length pdfPillars ||
        length cdfPillars == length pdfPillars + 1)
        &&
        (all (`elem` cdfPillars) pdfPillars &&
        all (`elem` pdfPillars) cdfPillars)

-- | Ensure that the pdf construction is stable
propPdfStable :: Test
propPdfStable =
  TestQC $ run $ forAll genPdfPillars $ \ pdfP ->
    mkPdf pdfP == mkPdf ( reverse pdfP )

-- | Ensure that the pdf probabilities are correct.
propPdfConsistency :: Test
propPdfConsistency =
  TestQC $ run $ \(PDF pdfP) ->
    Map.foldl (\acc x -> x + acc) 0 pdfP <= 1

-- | Ensure that the inverseCdf recovers the correct pdf pillars
propInvCdfValid :: Test
propInvCdfValid =
  TestQC $ run $ forAll genInvCdfQuery $ \(pdf, xs) ->
    let cdf = mkCdf pdf
        set = Set.fromList $ Nothing : map Just (fst $ unzip $ Map.toList $ unPDF pdf)
     in all (==True) $
           map (\ x -> inverseCdf cdf x `Set.member` set) xs

-- | Ensure that the PDF construction fails when expected
testPdfFail :: Test
testPdfFail =
  TestPure $ const $
      negativePro && nullPro && greaterPro && nullPro2
  where
      negativePro = isLeft $ mkPdf [(1, -1)]
      nullPro = isLeft $ mkPdf []
      nullPro2 = isLeft $ mkPdf [(1,0)]
      greaterPro = isLeft $ mkPdf [(1, 0.4), (2, 0.5), (3, 0.4)]

-- | Ensure that the CDF fails with incorrect PDF
testCdfFail :: Test
testCdfFail =
  TestIO $ failTest $
    mkCdf $ PDF $ Map.fromList [(1, 0.5), (2, 0.3), (3, 0.3)]

-- | Ensure that inverse CDF fails when expected
testInvCdfFail :: Test
testInvCdfFail =
  TestIO $ failTest $
    let cdf = CDF $ Map.fromList [(0.5, Just 1), (0.8, Just 2), (1, Just 3)]
    in inverseCdf cdf 1.1

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
      equal :: Ord a => Map.Map a Double -> Map.Map a Double -> Bool
      equal lhs rhs =
          Map.foldl (\ acc x -> if not acc then False else x < 0.0001) True $
              Map.unionWith (\ x y -> abs (x - y)) lhs rhs

      pdfP = [(1, 0.2), (1, 0.1), (2, 0.4), (3, 0.3),(4, 0)]
      res  = mkPdf pdfP
      test = case res of
              Left _ -> False
              Right pdf ->
                unPDF pdf `equal` (Map.fromList [(1, 0.3), (2, 0.4), (3, 0.3)])

-- | Non regression test for the cdf
testCDFNoRegression :: Test
testCDFNoRegression =
  TestPure $ const test
    where
      equal :: CdfPillars -> CdfPillars -> Bool
      equal lhs rhs
        | length lhs /= length rhs = False
        | otherwise = all (==True) $
              zipWith (\ (p1, v1) (p2, v2) -> abs (p1 - p2) < 0.0001 && v1 == v2) lhs rhs
      pdfP = [(1, 0.2), (1, 0.1), (2, 0.4), (3, 0.3),(4, 0)]
      res  = mkPdf pdfP
      test = case res of
              Left _ -> False
              Right pdf ->
                Map.toList (unCDF $ mkCdf pdf)
                    `equal` [(0.3, Just 1), (0.7, Just 2), (1, Just 3)]

