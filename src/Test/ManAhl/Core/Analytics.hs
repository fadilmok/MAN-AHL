{-# LANGUAGE BangPatterns #-}
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
tests :: [(String, IO Bool)]
tests = [
    ("CDF Creation",              run propCdfFromPdfComplete)
   ,("CDF and PDF pillars check", run propCdfPdfPillars)
   ,("PDF stable with reverse Pillars",
        run $ forAll genPdfPillars propPdfStable)
   ,("PDF consistency",           run propPdfConsistency)
   ,("InvCDF valid",
        run $ forAll genInvCdfQuery propInvCdfValid)
   ,("PDF Failure",               testPdfFail)
   ,("CDF Failure",               testCdfFail)
   ,("Inverse CDF Failure",       testInvCdfFail)
   ,("Mean Failure",              testMeanFail)
   ,("Std Failure",               testStdFail)
   ,("PDF No regression",         testPDFNoRegression)
   ,("CDF No regression",         testCDFNoRegression)
  ]

-- | Ensure that the last pillars of the CDF is 100%
propCdfFromPdfComplete :: PDF -> Bool
propCdfFromPdfComplete pdf = fst (last pillarsCdf) == 1
  where
    cdf = mkCdf pdf
    pillarsCdf = Map.toList $ unCDF cdf

-- | Ensure that the PDF and CDF have the correct number of pillars
propCdfPdfPillars :: PDF -> Bool
propCdfPdfPillars pdf =
    nCdfPillars == nPdfPillars || nCdfPillars == nPdfPillars + 1
  where
    nCdfPillars = Map.size $ unCDF $ mkCdf pdf
    nPdfPillars = Map.size $ unPDF pdf

-- | Ensure that the pdf construction is stable
propPdfStable :: PdfPillars -> Bool
propPdfStable pdfP = mkPdf pdfP == mkPdf ( reverse pdfP )

-- | Ensure that the pdf probabilities are correct.
propPdfConsistency :: PDF -> Bool
propPdfConsistency (PDF pdfP) = Map.foldl (\acc x -> x + acc) 0 pdfP <= 1

-- | Ensure that the inverseCdf recovers the correct pdf pillars
propInvCdfValid :: (PDF, [Double]) -> Bool
propInvCdfValid (pdf, xs) = all (==True) $
    map (\ x -> inverseCdf cdf x `Set.member` set) xs
  where
    cdf = mkCdf pdf
    set = Set.fromList $ Nothing : map Just (fst $ unzip $ Map.toList $ unPDF pdf)

-- | Ensure that the PDF construction fails when expected
testPdfFail :: IO Bool
testPdfFail = do
  let negativePro = isLeft $ mkPdf [(1, -1)]
      nullPro = isLeft $ mkPdf []
      nullPro2 = isLeft $ mkPdf [(1,0)]
      greaterPro = isLeft $ mkPdf [(1, 0.4), (2, 0.5), (3, 0.4)]
      res = negativePro && nullPro && greaterPro && nullPro2

  putStrLn $ "Test " ++ if res then "Passed" else "FAILED"
  return res

-- | Ensure that the CDF fails with incorrect PDF
testCdfFail :: IO Bool
testCdfFail =
  failTest $ do
    let fakePdf = PDF $ Map.fromList [(1, 0.5), (2, 0.3), (3, 0.3)]
        !c = mkCdf fakePdf
    return ()

-- | Ensure that inverse CDF fails when expected
testInvCdfFail :: IO Bool
testInvCdfFail =
  failTest $ do
    let cdf = CDF $ Map.fromList [(0.5, Just 1), (0.8, Just 2), (1, Just 3)]
        !c = inverseCdf cdf 1.1
    return ()

-- | Ensure that the mean fails when expected
testMeanFail :: IO Bool
testMeanFail =
  failTest $ do
    let !c = mean []
    return ()

-- | Ensure that the stdDev fails when expected
testStdFail :: IO Bool
testStdFail =
  failTest $ do
    let !c = stdDev []
    return ()

-- | Non regression test for the pdf
testPDFNoRegression :: IO Bool
testPDFNoRegression = do
  let equal :: Ord a => Map.Map a Double -> Map.Map a Double -> Bool
      equal lhs rhs =
          Map.foldl (\ acc x -> if not acc then False else x < 0.0001) True $
              Map.unionWith (\ x y -> abs (x - y)) lhs rhs

      pdfP = [(1, 0.2), (1, 0.1), (2, 0.4), (3, 0.3),(4, 0)]
      res  = mkPdf pdfP
      test = case res of
              Left _ -> False
              Right pdf ->
                unPDF pdf `equal` (Map.fromList [(1, 0.3), (2, 0.4), (3, 0.3)])
  putStrLn $ "Test " ++ if test then "Passed" else "FAILED"
  return test

-- | Non regression test for the cdf
testCDFNoRegression :: IO Bool
testCDFNoRegression = do
  let equal :: CdfPillars -> CdfPillars -> Bool
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
  putStrLn $ "Test " ++ if test then "Passed" else "FAILED"
  return test

