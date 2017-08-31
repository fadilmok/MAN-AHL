{-# LANGUAGE TupleSections, RecordWildCards #-}
-- | Module containing the simple analytics
module ManAhl.Core.Analytics(
  mkPdf, mkCdf,
  inverseCdf,
  mean, stdDev,
  probaFromCount
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
mkPdf :: PdfPillars -> Either String PDF
mkPdf xs
  | null xs = Left "The pdf pillars are empty."
  | null $ filter (\(_, x) -> x /= 0) xs =
      Left "The pdf pillars contain only zero."
  | foldl (\ acc (_, x) -> if acc then acc else x < 0) False xs =
      Left "PDF Pillars contain negative values."
  | foldl (\ acc (_, x) -> acc + x) 0 xs > 1 =
      Left "The sum of PDF probabilities are greater than 1."
  | otherwise = Right $ PDF $
      foldl (\ m (v, p) -> if p == 0 then m else Map.insertWith (+) v p m)
        Map.empty xs

-- | Create a discrete cumulative function,
-- it assumes that the pdf is correct and
-- fails if the cumulated probabilities are greater than 1.
mkCdf :: PDF -> CDF
mkCdf (PDF m) = CDF $
    Map.fromAscList $ reverse $ check $ Map.foldlWithKey go [] m
  where
    go :: CdfPillars -> Int -> Double -> CdfPillars
    go [] x p = [(p, Just x)]
    go (y:ys) x p = (p + fst y, Just x) : y : ys

    check :: CdfPillars -> CdfPillars
    check [] = [(1, Nothing)]
    check vals@((p, _):xs)
      | p < 1 = (1, Nothing) : vals
      | p == 1 = vals
      | otherwise = error "Cumulative probabilities cannot exceed 1"

-- | Inverse cdf function,
-- it assumes that the inputs are between 0 and 1 and that
-- the cdf is correct, it fails otherwise.
inverseCdf :: CDF -> Double -> Maybe Int
inverseCdf (CDF m) n
  = case n `Map.lookupGE` m of
      Just (_, v) -> v
      Nothing -> error $ "InverseCDF fails: " ++ show n ++ " CDF: " ++ show m

-- | Compute the probabilities from the result distribution frequency
probaFromCount :: Stats a -> Map a Double
probaFromCount (Stats cs n)
  = Map.map (\ x -> fromIntegral x / fromIntegral n) cs

-- | Compute the mean for a non empty list
mean :: (Num a, Fractional a) => [a] -> a
mean [] = error "The input list is empty"
mean xs = sum xs / fromIntegral (length xs)

-- | Compute the standard deviation for a non empty list
stdDev :: (Num a, Floating a, Fractional a) => [a] -> a
stdDev [] = error "The input list is empty"
stdDev xs = sqrt $
    ( sum $ map (\x -> (x - avg)^2) xs ) / fromIntegral ( length xs )
  where avg = mean xs
