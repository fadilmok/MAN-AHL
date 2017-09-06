{-# LANGUAGE BangPatterns #-}
module ManAhl.Core.Statistics (
  computeStats
) where

import ManAhl.Core.Types
import Control.Monad.State.Strict

computeStat :: (Ord b, Show b, ProbaEngine a b c) =>
  a b -> c -> UniformRNG -> Int -> Stats b
computeStat engine params rng n = -- statistics (emptyCurve) $
  flip evalState
    (Stats emptyCurve 0 Nothing Nothing Nothing Nothing Nothing Nothing, engine) $
      mkStats params rng n

mkStats :: (Ord b, Show b, ProbaEngine a b c) =>
  c -> UniformRNG -> Int -> State (Stats b, a b) (Stats b)
mkStats params rng 1 = do
   (stats, e) <- get
   let (!y, e') = computeProba' params rng e
       !stats' = stats {
                 hsDistri = addWith (+) (fst $ (hsDistri stats) !!! y) 1 $
                     hsDistri stats
                ,hsCount = hsCount stats + 1
              }
   put (stats', e')
   return stats'
mkStats params rng n = allStats' (n - 1) $ mkStats params rng 1
  where
     allStats' 0 acc = acc
     allStats' !n acc = allStats' (n - 1) $ acc >> mkStats params rng 1
