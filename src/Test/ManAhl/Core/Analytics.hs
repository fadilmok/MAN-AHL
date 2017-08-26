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
  arbitrary = do
    n <- choose (1, 10) :: Gen Int
    PDF `liftM` ( suchThat (
        snd `liftM` foldlM (\ (l, xs) _ -> do
          i <- arbitrary
          x <- choose (0.01, max 0 $ 1 - l)
          return ( x + l, (i, x):xs ) ) (0, []) [1 .. n]
      )
      $ \xs -> let s = sum (snd $ unzip xs) in s > 0 && s <= 1 )

run :: Testable prop => prop -> IO()
run p = quickCheckWith stdArgs{ maxSuccess = 10000 } p

tests :: [( String, IO()) ]
tests = [
    ("CDF Creation", run prop_cdfFromPdfComplete)
   ,("CDF and PDF pillars check", run prop_cdfPdfPillars)
   ,("CDF stable for various identical PDF", run prop_cdfStable)
  ]

prop_cdfFromPdfComplete :: PDF -> Bool
prop_cdfFromPdfComplete pdf = fst (last pillarsCdf) == 1
  where
    cdf = mkCdf pdf
    pillarsCdf = Map.toList $ unCDF cdf

prop_cdfPdfPillars :: PDF -> Bool
prop_cdfPdfPillars pdf =  nCdfPillars == nPdfPillars || nCdfPillars == nPdfPillars + 1
  where
    nCdfPillars = Map.size $ unCDF $ mkCdf pdf
    nPdfPillars = length $ filter (/= 0) $ nub $ sort $ snd $ unzip $ unPDF pdf

prop_cdfStable :: PDF -> Bool
prop_cdfStable pdf = mkCdf pdf == mkCdf ( PDF $ reverse $ unPDF pdf )

cdfConstructionFail :: IO Bool
cdfConstructionFail = undefined

pdfConstructionFail :: IO Bool
pdfConstructionFail = undefined

