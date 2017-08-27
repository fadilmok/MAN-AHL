module ManAhl.Core.Random(
  -- * Creation
  mkUniformRNG,
  -- * Generation
  next,
  nexts
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

nexts :: UniformRNG -> Int -> ([Double], UniformRNG)
nexts rng n = (fst $ unzip rs, rng')
  where rs@((_,rng'):_) = foldl go [] [1..n]
        go [] _ = [next rng]
        go (x@(_,r):xs) _ = next r : x : xs

