{-# LANGUAGE TupleSections, RecordWildCards #-}
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

inverseCdf :: CDF -> Double -> Maybe Int
inverseCdf (CDF m) n
  = case n `Map.lookupGE` m of
      Just (x, v) -> v
      Nothing -> error $ "InverseCDF fails: " ++ show n ++ " CDF: " ++ show m

probaFromCount :: Stats a -> Map a Double
probaFromCount (Stats cs n)
  = Map.map (\ x -> fromIntegral x / fromIntegral n) cs

mean :: (Num a, Fractional a) => [a] -> a
mean [] = error "The input list is empty"
mean xs = sum xs / fromIntegral (length xs)

stdDev :: (Num a, Floating a, Fractional a) => [a] -> a
stdDev [] = error "The input list is empty"
stdDev xs = sqrt $
    ( sum $ map (\x -> (x - avg)^2) xs ) / fromIntegral ( length xs )
  where avg = mean xs
