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

-- | Make UniformRNG instance of RandGen by delegating the
-- work to the inner RNG
instance RandomGen UniformRNG where
  next (RandomEcuyer rng)   = let (x, r) = next rng in (x, RandomEcuyer r)
  next (RandomMersenne rng) = let (x, r) = next rng in (x, RandomMersenne r)
  genRange (RandomEcuyer rng)   = genRange rng
  genRange (RandomMersenne rng) = genRange rng
  split (RandomEcuyer rng)   = let (g1, g2) = split rng in (RandomEcuyer g1, RandomEcuyer g1)
  split (RandomMersenne rng) = let (g1, g2) = split rng in (RandomMersenne g1, RandomMersenne g2)

-- Create Uniform RNG encapsulating Mersenne or Ecuyer
mkUniformRNG :: Maybe UniformRNGType -> IO UniformRNG
mkUniformRNG (Just Ecuyer)   = return . RandomEcuyer =<< newStdGen
mkUniformRNG (Just Mersenne) = return . RandomMersenne =<< newPureMT
mkUniformRNG Nothing         = mkUniformRNG $ Just Mersenne

-- Compute bounded uniform probabilities
runProbaUni :: Maybe UniformRNGType -> ProbaUniEngine a -> IO a
runProbaUni rT e = do
  uniRng <- mkUniformRNG rT
  return $ evalState e uniRng

-- Compute the cumulative statistics for the uniform probabilities
runStatUni :: Maybe UniformRNGType -> StatUniEngine -> IO (Stats Double)
runStatUni rT e = do
  uniRng <- mkUniformRNG rT
  return $ flip evalState uniRng $
    evalStateT e (Stats statRange 0)

-- | Engine to compute the next bounded uniform probability
nextVal :: ProbaUniEngine Double
nextVal = state $! randomR (0, 1)

-- | Engine to compute n next bounded uniform probabilities
nextVals :: Int -> ProbaUniEngine [Double]
nextVals n = replicateM n nextVal

-- | Engine to compute the next cumulative stastitics for
-- a bounded uniform distribution
nextUStat :: StatUniEngine
nextUStat = do
  uniRng <- lift get
  let (!x, !r) = runState nextVal uniRng
  lift $ put r
  Stats cs n <- get
  let !stats = Stats {
          hsCount = Map.insertWith (+)
            (case x `Map.lookupGE` statRange of
               Just y -> fst y; Nothing -> error "Failure") 1 cs
         ,hsTotalCount = n + 1
        }
  put stats
  return stats

-- | Engine to compute the cumulative statistics for n
-- weighted probabilities
-- It fails if n is 0.
allUStats :: Int -> StatUniEngine
allUStats 0 = error "You need at least one element"
allUStats n = allStats' (n-1) nextUStat
  where
    allStats' :: Int -> StatUniEngine -> StatUniEngine
    allStats' 1 acc = acc
    allStats' !n acc = allStats' (n - 1) $ acc >> nextUStat

statRange :: Map Double Int
statRange = Map.fromList $ zip (map (/100) [1, 2 .. 100]) [0..]
