{-# LANGUAGE BangPatterns #-}
module ManAhl.Core.Random(
  -- * Creation
  mkUniformRNG,
  -- ** Probabilities
  nextVal, nextVals,
  -- ** Statistics
  nextUStat, allUStats,
  -- * Execution
  runProbaUni,
  runStatUni
) where

import ManAhl.Core.Types
import Control.Monad
import Control.Monad.State.Strict
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random
import System.Random.Mersenne.Pure64

instance RandomGen UniformRNG where
  next (RandomEcuyer rng)   = let (x, r) = next rng in (x, RandomEcuyer r)
  next (RandomMersenne rng) = let (x, r) = next rng in (x, RandomMersenne r)
  genRange (RandomEcuyer rng)   = genRange rng
  genRange (RandomMersenne rng) = genRange rng
  split (RandomEcuyer rng)   = let (g1, g2) = split rng in (RandomEcuyer g1, RandomEcuyer g1)
  split (RandomMersenne rng) = let (g1, g2) = split rng in (RandomMersenne g1, RandomMersenne g2)

mkUniformRNG :: Maybe UniformRNGType -> IO UniformRNG
mkUniformRNG (Just Ecuyer)   = return . RandomEcuyer =<< newStdGen
mkUniformRNG (Just Mersenne) = return . RandomMersenne =<< newPureMT
mkUniformRNG Nothing         = mkUniformRNG $ Just Mersenne

runProbaUni :: Maybe UniformRNGType -> ProbaUniEngine a -> IO a
runProbaUni rT e = do
  uniRng <- mkUniformRNG rT
  return $ evalState e uniRng

runStatUni :: Maybe UniformRNGType -> StatUniEngine -> IO (Stats Double)
runStatUni rT e = do
  uniRng <- mkUniformRNG rT
  return $ flip evalState uniRng $
    evalStateT e (Stats statRange 0)

nextVal :: ProbaUniEngine Double
nextVal = state $! randomR (0, 1)

nextVals :: Int -> ProbaUniEngine [Double]
nextVals n = replicateM n nextVal

nextUStat :: StatUniEngine
nextUStat = do
  uniRng <- lift get
  let (!x, !r) = runState nextVal uniRng
  lift $ put r
  Stats !cs !n <- get
  let !stats = Stats {
          hsCount = Map.insertWith (+)
            (case x `Map.lookupGE` statRange of
               Just y -> fst y; Nothing -> error "Failure") 1 cs
         ,hsTotalCount = n + 1
        }
  put stats
  return stats

allUStats :: Int -> StatUniEngine
allUStats 0 = error "You need at least one element"
allUStats n = foldl' (\ !acc !x -> acc >> x) nextUStat $
  replicate (n-1) nextUStat

statRange :: Map Double Int
statRange = Map.fromList $ zip (map (/100) [1, 2 .. 100]) [0..]
