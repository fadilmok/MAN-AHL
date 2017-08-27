{-# LANGUAGE TupleSections, RecordWildCards #-}
module ManAhl.Core.Analytics(
  mkPdf, mkCdf,
  inverseCdf,
  mkHistogram,
  mean, stdDev
) where

import Data.Function (on)
import Data.List
import qualified Data.Map as Map
import ManAhl.Core.Types

mkPdf :: PdfPillars -> PDF
mkPdf xs = PDF $ prepare xs
  where
    prepare :: PdfPillars -> PdfPillars
    prepare xs = filter (\(_, x) -> x /= 0) $
      nubBy ((==) `on` snd) $ sortBy (compare `on` snd) xs

mkCdf :: PDF -> CDF
mkCdf (PDF xs) = CDF $ Map.fromAscList $ reverse $ check $ foldl go [] xs
  where
    go :: CdfPillars -> (Int, Double) -> CdfPillars
    go [] (x,p) = [(p, Just x)]
    go (y:ys) x = (snd x + fst y, Just $ fst x) : y : ys

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

mkHistogram :: [Maybe Int] -> Histogram
mkHistogram xs = Histogram{..}
  where
    hsRaw = Map.toList . Map.fromListWith (+) . map (,1) $ xs
    hsTotalCount = length xs
    hsStat = map (\(i, x) -> (i, fromIntegral x / fromIntegral hsTotalCount)) hsRaw

mean :: (Num a, Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

stdDev :: (Num a, Floating a, Fractional a) => [a] -> a
stdDev xs = sqrt $
    ( sum $ map (\x -> (x - avg)^2) xs ) / fromIntegral ( length xs )
  where avg = mean xs
