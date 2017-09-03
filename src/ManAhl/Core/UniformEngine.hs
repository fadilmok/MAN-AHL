{-# LANGUAGE BangPatterns, MultiParamTypeClasses  #-}
module ManAhl.Core.UniformEngine(
  -- * Creation
  mkUniformRNG,
  -- * Engines
  ProbaUniEngine(),
  StatUniEngine()
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

instance ProbaEngine ProbaUniEngine Double where
  computeProba _ r e = evalState (unPUIE e) r

  nextNum = state $! randomR (0, 1)

instance StatEngine StatUniEngine where
  computeStats _ r e = let
      s = evalState (unSUE e) (UniStats doubleDistEmpty 0 Nothing, r)
   in s{ hsProbaWP = Just $ probabilities $ hsDistriWP s }

  nextStat = do
    (UniStats cs n _, uniRng) <- get
    let (!x, !r) = runState (unPUIE nextNum) uniRng
        !stats = UniStats {
            hsDistriUni   = add x cs
           ,hsTotalCount  = n + 1
           ,hsProbaUni    = Nothing
          }
    put (stats, r)
    return stats

