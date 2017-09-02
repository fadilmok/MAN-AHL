{-# OPTIONS_GHC -O2 #-}
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
import ManAhl.Core.Analytics

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
mkUniformRNG :: UniformRNGType -> IO UniformRNG
mkUniformRNG Ecuyer   = return . RandomEcuyer =<< newStdGen
mkUniformRNG Mersenne = return . RandomMersenne =<< newPureMT

-- Compute bounded uniform probabilities
runProbaUni :: UniformRNG -> ProbaUniEngine a -> a
runProbaUni r e = evalState e r

-- Compute the cumulative statistics for the uniform probabilities
runStatUni :: UniformRNG -> StatUniEngine -> Stats Double
runStatUni r e =
  flip evalState r $
    evalStateT e $ Stats doubleDistEmpty 0

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
          hsDistri = add cs x
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
    allStats' 0 acc = acc
    allStats' !n acc = allStats' (n - 1) $ acc >> nextUStat

