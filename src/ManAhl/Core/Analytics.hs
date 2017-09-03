{-# LANGUAGE TupleSections, RecordWildCards, FlexibleInstances #-}
-- | Module containing simple analytics
module ManAhl.Core.Analytics(
  -- * Creation
  mkPdf, mkCdf, mkInvCdf,
  -- * Access
  invCdf,(!!!),
  -- * Statistics
  mean, stdDev,
  probabilities
) where

import Data.Function (on)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import ManAhl.Core.Types

-- | Creates discrete probability function.
-- The pillars cannot be null, contain negative probability,
-- the sum of the probability cannot exceed 1.
-- The pillars with probability 0 are discarded.
-- O(n log n)
mkPdf :: PdfPillars -> Either String PDF
mkPdf (PdfPillars xs)
  | null xs = Left "The pdf pillars are empty."
  | null $ filter (\(_, x) -> x /= 0) xs =
      Left "The pdf pillars contain only zero."
  | foldl (\ acc (_, x) -> if acc then acc else x < 0 || x > 1) False xs =
      Left "PDF Pillars contain negative values or greater than 1."
  | foldl (\ acc (_, x) -> acc + x) 0 xs > 1 =
      Left "The sum of PDF probabilities are greater than 1."
  | otherwise = Right $
      let m = foldl (\ m (v, p) ->
            if p == 0 then m else addWith (+) (Just v) p m) emptyCurve xs
          s = foldl (\acc (_, x)-> acc + x) 0 xs
       in (if s < 1 then add Nothing (1 - s) else id) m

-- | Create a discrete cumulative function,
-- it assumes the pdf is correct
-- O(n)
mkCdf :: PDF -> CDF
mkCdf = fromPillars . Map.foldlWithKey go [] . toRaw
  where
    go [] x p = [(x, p)]
    go (y:ys) x p = (x, p + snd y) : y : ys

-- | Create a discrete inverse cumulative function
-- it assumes the cdf is correct
-- O(n)
mkInvCdf :: CDF -> InvCDF
mkInvCdf = fromPillars . Prelude.map (\(x, y) -> (y, x)) . toPillars

-- | Inverse cdf function,
-- it assumes that the inputs are between 0 and 1 and that
-- the invCdf is correct, it fails otherwise.
-- O(log n)
invCdf :: InvCDF -> Double -> Maybe Int
invCdf (InvCDF m) x = snd $ m !!! x

-- | Compute the probabilities from the result distribution
-- O(n)
probabilities :: Distribution a -> [(a, Double)]
probabilities dist
  = toPillars $ fmap (\ x -> fromIntegral x / fromIntegral n) $ unDist dist
    where n = cFoldl (\acc x -> acc + x) 0 dist

-- | Compute the mean for a non empty list
-- O(n)
mean :: (Num a, Fractional a) => [a] -> a
mean [] = error "The input list is empty"
mean xs = sum xs / fromIntegral (length xs)

-- | Compute the standard deviation for a non empty list
-- O(n)
stdDev :: (Num a, Floating a, Fractional a) => [a] -> a
stdDev [] = error "The input list is empty"
stdDev xs = sqrt $
    ( sum $ map (\x -> (x - avg)^2) xs ) / fromIntegral ( length xs )
  where avg = mean xs
