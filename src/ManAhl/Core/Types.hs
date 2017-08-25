module ManAhl.Core.Types(
  Engine(..),
  UniformRNGType(..),
  UniformRNG(..)
) where

import qualified System.Random as Ecuyer
import qualified System.Random.Mersenne.Pure64 as Mersenne

data UniformRNG
  = RandomEcuyer Ecuyer.StdGen
  | RandomMersenne Mersenne.PureMT

data UniformRNGType = Ecuyer | Mersenne

data Engine = Engine{
    pillars    :: [(Int, Double)]
   ,uniformRng :: UniformRNG
  }
