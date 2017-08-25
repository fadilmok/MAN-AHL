module ManAhl.Core.Random(
  -- * Creation
  mkUniformRNG,
  -- * Generation
  next
) where

import ManAhl.Core.Types

import System.Random hiding (next)
import System.Random.Mersenne.Pure64

mkUniformRNG :: Maybe UniformRNGType -> IO UniformRNG
mkUniformRNG (Just Ecuyer)   = return . RandomEcuyer =<< newStdGen
mkUniformRNG (Just Mersenne) = return . RandomMersenne =<< newPureMT
mkUniformRNG Nothing         = mkUniformRNG $ Just Mersenne

next :: UniformRNG -> (Double, UniformRNG)
next (RandomEcuyer rng)   = let (x, r) = randomR (0, 1) rng in (x, RandomEcuyer r)
next (RandomMersenne rng) = let (x, r) = randomR (0, 1) rng in (x, RandomMersenne r)
