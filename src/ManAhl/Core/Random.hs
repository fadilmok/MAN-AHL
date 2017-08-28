module ManAhl.Core.Random(
  -- * Creation
  mkUniformRNG,
  -- * Generation
  next', nexts',
  nextVal, nextVals
) where

import ManAhl.Core.Types
import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.Mersenne.Pure64

mkUniformRNG :: Maybe UniformRNGType -> IO UniformRNG
mkUniformRNG (Just Ecuyer)   = return . RandomEcuyer =<< newStdGen
mkUniformRNG (Just Mersenne) = return . RandomMersenne =<< newPureMT
mkUniformRNG Nothing         = mkUniformRNG $ Just Mersenne

instance RandomGen UniformRNG where
  next (RandomEcuyer rng)   = let (x, r) = next rng in (x, RandomEcuyer r)
  next (RandomMersenne rng) = let (x, r) = next rng in (x, RandomMersenne r)
  split = undefined

next' :: State UniformRNG Double
next' = state $ randomR (0, 1)

nexts' :: Int -> State UniformRNG [Double]
nexts' n = replicateM n next'

nextVal :: UniformRNG -> Double
nextVal rng = evalState next' rng

nextVals :: UniformRNG -> Int -> [Double]
nextVals rng n = evalState (nexts' n) rng
