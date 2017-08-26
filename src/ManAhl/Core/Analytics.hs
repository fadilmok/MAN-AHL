module ManAhl.Core.Analytics(
  mkCdf,
  inverseCdf
) where

import qualified Data.Map as Map
import ManAhl.Core.Types

mkCdf :: PDF -> CDF
mkCdf (PDF xs) = CDF $ Map.fromAscList $ reverse $ check $ foldl go [] xs
  where
    go :: [(Int, Double)] -> (Int, Double) -> [(Int, Double)]
    go [] x = [x]
    go (y:ys) x = (fst x, snd x + snd y) : y : ys
    check :: [(Int, Double)] -> [(Double, Maybe Int)]
    check [] = [(1, Nothing)]
    check vals@((_,p):xs)
      | p < 1 = (1, Nothing) : map (\ (x,y) -> (y, Just x)) vals
      | p == 1 = map (\ (x,y) -> (y, Just x)) vals
      | otherwise = error "Cumulative probabilities cannot exceed 1"

inverseCdf :: CDF -> Double -> Maybe Int
inverseCdf (CDF m) n = case n `Map.lookupGE` m of
                         Just (x, v) -> v
                         Nothing -> error $ "InverseCDF fails: " ++ show n ++ " CDF: " ++ show m

