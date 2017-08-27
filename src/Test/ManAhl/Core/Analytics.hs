module Test.ManAhl.Core.Analytics(
  tests,
  genPdfPillars
) where

import ManAhl.Core.Analytics
import ManAhl.Core.Types
import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck
import Test.ManAhl.QuickCheck

instance Arbitrary PDF where
  arbitrary = mkPdf `liftM` genPdfPillars

instance Arbitrary CDF where
  arbitrary = mkCdf `liftM` arbitrary

genPdfPillars :: Gen PdfPillars
genPdfPillars = do
    n <- choose (1, 10) :: Gen Int
    suchThat (
        snd `liftM` foldlM (\ (l, xs) _ -> do
          i <- choose (-1000, 1000)
          x <- choose (0.01, max 0.02 $ 1 - l)
          return ( x + l, (i, x):xs ) ) (0, []) [1 .. n]
      )
      $ \xs -> let s = sum (snd $ unzip xs) in s > 0 && s <= 1

genInvCdfQuery :: Gen (PDF, [Double])
genInvCdfQuery = do
  pdf <- arbitrary
  n <- choose (1, 1000)
  xs <- replicateM n $ choose(0, 1)
  return (pdf, xs)

tests :: [(String, IO Bool)]
tests = [
    ("CDF Creation", run propCdfFromPdfComplete)
   ,("CDF and PDF pillars check", run propCdfPdfPillars)
   ,("PDF stable with reverse Pillars",
        run $ forAll genPdfPillars propPdfStable)
   ,("PDF consistency", run propPdfConsistency)
   ,("Histo creation", run propMkHisto)
   ,("InvCDF valid",
        run $ forAll genInvCdfQuery propInvCdfValid)
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
propPdfConsistency (PDF pdfP) = sum (snd $ unzip $ Map.toList pdfP) <= 1

propMkHisto :: [Maybe Int] -> Bool
propMkHisto xs = hsTotalCount hist == sum ( snd $ unzip $ hsRaw hist)
  where
    hist = mkHistogram xs

propInvCdfValid :: (PDF, [Double]) -> Bool
propInvCdfValid (pdf, xs) = all (==True) $
    map (\ x -> inverseCdf cdf x `Set.member` set) xs
  where
    cdf = mkCdf pdf
    set = Set.fromList $ Nothing : map Just (fst $ unzip $ Map.toList $ unPDF pdf)

cdfConstructionFail :: IO Bool
cdfConstructionFail = undefined

pdfConstructionFail :: IO Bool
pdfConstructionFail = undefined
