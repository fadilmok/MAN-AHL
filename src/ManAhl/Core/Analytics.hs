{-# LANGUAGE TupleSections #-}
module ManAhl.Core.Analytics(
  mkCdf,
  inverseCdf,
  mkHistogram
) where

import qualified Data.Map as Map
import ManAhl.Core.Types

mkCdf :: PDF -> CDF
mkCdf (PDF xs) = CDF $ Map.fromAscList $ reverse $ check $ foldl go [] xs
  where

    go :: [(Double, Maybe Int)] -> (Int, Double) -> [(Double, Maybe Int)]
    go [] (x,p) = [(p, Just x)]
    go (y:ys) x = (snd x + fst y, Just $ fst x) : y : ys

    check :: [(Double, Maybe Int)] -> [(Double, Maybe Int)]
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

mkHistogram :: [Maybe Int] -> Map.Map (Maybe Int) Int
mkHistogram = Map.fromListWith (+) . map (,1)
