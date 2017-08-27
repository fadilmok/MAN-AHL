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
    ("CDF Creation", run prop_cdfFromPdfComplete)
   ,("CDF and PDF pillars check", run prop_cdfPdfPillars)
   ,("PDF stable with reverse Pillars",
        run $ forAll genPdfPillars prop_pdfStable)
   ,("PDF consistency", run prop_pdfConsistency)
   ,("InvCDF Monotonus", run prop_invCdfMonotonus)
  ]

prop_cdfFromPdfComplete :: PDF -> Bool
prop_cdfFromPdfComplete pdf = fst (last pillarsCdf) == 1
  where
    cdf = mkCdf pdf
    pillarsCdf = Map.toList $ unCDF cdf

prop_cdfPdfPillars :: PDF -> Bool
prop_cdfPdfPillars pdf =
    nCdfPillars == nPdfPillars || nCdfPillars == nPdfPillars + 1
  where
    nCdfPillars = Map.size $ unCDF $ mkCdf pdf
    nPdfPillars = length $ unPDF pdf

prop_pdfStable :: PdfPillars -> Bool
prop_pdfStable pdfP = mkPdf pdfP == mkPdf ( reverse pdfP )

prop_pdfConsistency :: PDF -> Bool
prop_pdfConsistency (PDF pdfP) = sum (snd $ unzip pdfP) <= 1

cdfConstructionFail :: IO Bool
cdfConstructionFail = undefined

pdfConstructionFail :: IO Bool
pdfConstructionFail = undefined

prop_invCdfMonotonus :: CDF -> Bool
prop_invCdfMonotonus cdf = all (==True) $
  zipWith (\x y -> inverseCdf cdf x <= inverseCdf cdf y)
    (init [0, 0.05..1]) $ tail [0, 0.05..1]
