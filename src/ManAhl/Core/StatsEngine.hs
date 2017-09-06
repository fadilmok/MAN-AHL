{-# LANGUAGE BangPatterns #-}
module ManAhl.Core.StatsEngine (
  computeStats
) where

import ManAhl.Core.Types
import ManAhl.Core.Analytics

import Control.Monad.State.Strict
import qualified Data.Map as Map

computeStats :: (Ord b, Show b, ProbaEngine a b c) =>
  c -> UniformRNG -> Int -> a b -> (CollectStats b, FinalStats b)
computeStats params rng n engine =
  let pdf = getPDF engine params
      d = mkInitialDistri pdf
   in statistics pdf $
        flip evalState
          (CollectStats d 0, engine) $
             mkStats params rng n

mkStats :: (Ord b, Show b, ProbaEngine a b c) =>
  c -> UniformRNG -> Int -> State (CollectStats b, a b) (CollectStats b)
mkStats _ _ 0 = error "Please query at least one stat"
mkStats params rng 1 = do
   (stats, e) <- get
   let (!y, e') = evalProba params rng e
       !stats' = stats {
                 csDistri = addWith (+) (fst $ (csDistri stats) !!! y) 1 $
                     csDistri stats
                ,csCount = csCount stats + 1
              }
   put (stats', e')
   return stats'
mkStats params rng n = allStats' (n - 1) $ mkStats params rng 1
  where
     allStats' 0 acc = acc
     allStats' !n acc = allStats' (n - 1) $ acc >> mkStats params rng 1

mkInitialDistri :: PDF a -> Distribution a
mkInitialDistri = fromRaw . Map.map (const 0 :: Double -> Int) . toRaw
