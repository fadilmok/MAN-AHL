{-# LANGUAGE BangPatterns #-}
module Test.ManAhl.Core.Analytics(
  tests,
  genPdfPillars
) where

import ManAhl.Core.Analytics
import ManAhl.Core.Types
import Control.Exception
import Control.Monad
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck
import Test.ManAhl.QuickCheck

instance Arbitrary PDF where
  arbitrary = do
      pillars <- genPdfPillars
      let pdf = mkPdf pillars
      return $ case pdf of
        Left s -> error $ "Generation failed " ++ s
        Right p -> p

instance Arbitrary CDF where
  arbitrary = mkCdf `liftM` arbitrary

genPdfPillars :: Gen PdfPillars
genPdfPillars = do
    n <- choose (1, 10) :: Gen Int
    suchThat (
        snd `liftM`
          foldlM (\ (l, xs) _ -> do
              i <- choose (-1000, 1000)
              x <- if l >= 1
                     then return 0
                     else choose (0.001, 1 - l)
              return ( x + l, (i, x):xs ) )
          (0, []) [1 .. n]
      )
      $ \xs -> let s = foldl (\acc (_, x) -> x + acc) 0 xs
                  in s > 0 && s <= 1

genInvCdfQuery :: Gen (PDF, [Double])
genInvCdfQuery = do
  pdf <- arbitrary
  n <- choose (1, 1000)
  xs <- replicateM n $ choose(0, 1)
  return (pdf, xs)

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

propCdfFromPdfComplete :: PDF -> Bool
propCdfFromPdfComplete pdf = fst (last pillarsCdf) == 1
  where
    cdf = mkCdf pdf
    pillarsCdf = Map.toList $ unCDF cdf

propCdfPdfPillars :: PDF -> Bool
propCdfPdfPillars pdf =
    nCdfPillars == nPdfPillars || nCdfPillars == nPdfPillars + 1
  where
    nCdfPillars = Map.size $ unCDF $ mkCdf pdf
    nPdfPillars = Map.size $ unPDF pdf

propPdfStable :: PdfPillars -> Bool
propPdfStable pdfP = mkPdf pdfP == mkPdf ( reverse pdfP )

propPdfConsistency :: PDF -> Bool
propPdfConsistency (PDF pdfP) = Map.foldl (\acc x -> x + acc) 0 pdfP <= 1

propInvCdfValid :: (PDF, [Double]) -> Bool
propInvCdfValid (pdf, xs) = all (==True) $
    map (\ x -> inverseCdf cdf x `Set.member` set) xs
  where
    cdf = mkCdf pdf
    set = Set.fromList $ Nothing : map Just (fst $ unzip $ Map.toList $ unPDF pdf)

testPdfFail :: IO Bool
testPdfFail = do
  let negativePro = isLeft $ mkPdf [(1, -1)]
      nullPro = isLeft $ mkPdf []
      nullPro2 = isLeft $ mkPdf [(1,0)]
      greaterPro = isLeft $ mkPdf [(1, 0.4), (2, 0.5), (3, 0.4)]
      res = negativePro && nullPro && greaterPro && nullPro2

  putStrLn $ "Test " ++ if res then "Passed" else "FAILED"
  return res

testCdfFail :: IO Bool
testCdfFail =
  failTest $ do
    let fakePdf = PDF $ Map.fromList [(1, 0.5), (2, 0.3), (3, 0.3)]
        !c = mkCdf fakePdf
    return ()

testInvCdfFail :: IO Bool
testInvCdfFail =
  failTest $ do
    let cdf = CDF $ Map.fromList [(0.5, Just 1), (0.8, Just 2), (1, Just 3)]
        !c = inverseCdf cdf 1.1
    return ()

testMeanFail :: IO Bool
testMeanFail =
  failTest $ do
    let !c = mean []
    return ()

testStdFail :: IO Bool
testStdFail =
  failTest $ do
    let !c = stdDev []
    return ()

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

