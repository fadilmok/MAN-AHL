module Test.ManAhl.Core.Analytics(
  tests
) where

import ManAhl.Core.Analytics
import ManAhl.Core.Types
import Test.QuickCheck
import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

instance Arbitrary PDF where
  arbitrary = mkPdf `liftM` genPdfPillars

instance Arbitrary CDF where
  arbitrary = mkCdf `liftM` arbitrary

genPdfPillars :: Gen PdfPillars
genPdfPillars = do
    n <- choose (1, 10) :: Gen Int
    suchThat (
        snd `liftM` foldlM (\ (l, xs) _ -> do
          i <- arbitrary
          x <- choose (0.01, max 0 $ 1 - l)
          return ( x + l, (i, x):xs ) ) (0, []) [1 .. n]
      )
      $ \xs -> let s = sum (snd $ unzip xs) in s > 0 && s <= 1

run :: Testable prop => prop -> IO()
run p = quickCheckWith stdArgs{ maxSuccess = 10000 } p

tests :: [( String, IO()) ]
tests = [
    ("CDF Creation", run propCdfFromPdfComplete)
   ,("CDF and PDF pillars check", run propCdfPdfPillars)
   ,("PDF stable with reverse Pillars",
        run $ forAll genPdfPillars propPdfStable)
   ,("PDF consistency", run propPdfConsistency)
   ,("Histo creation", run propMkHisto)
--   ,("InvCDF Monotonus", run prop_invCdfMonotonus)
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
    nPdfPillars = length $ unPDF pdf

propPdfStable :: PdfPillars -> Bool
propPdfStable pdfP = mkPdf pdfP == mkPdf ( reverse pdfP )

propPdfConsistency :: PDF -> Bool
propPdfConsistency (PDF pdfP) = sum (snd $ unzip pdfP) <= 1

cdfConstructionFail :: IO Bool
cdfConstructionFail = undefined

pdfConstructionFail :: IO Bool
pdfConstructionFail = undefined

propMkHisto :: [Maybe Int] -> Bool
propMkHisto xs = hsTotalCount hist == sum ( snd $ unzip $ hsRaw hist)
  where
    hist = mkHistogram xs
{-
prop_invCdfMonotonus :: CDF -> Bool
prop_invCdfMonotonus cdf = all (==True) $
  zipWith (\x y -> inverseCdf cdf x <= inverseCdf cdf y)
    (init [0, 0.05..1]) $ tail [0, 0.05..1]
-}
