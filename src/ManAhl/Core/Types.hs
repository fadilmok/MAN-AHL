module ManAhl.Core.Types(
  Engine(..),
  UniformRNGType(..),
  UniformRNG(..),
  CDF(..), PDF(..)
) where

import Data.Map
import qualified System.Random as Ecuyer
import qualified System.Random.Mersenne.Pure64 as Mersenne

data UniformRNG
  = RandomEcuyer Ecuyer.StdGen
  | RandomMersenne Mersenne.PureMT

data UniformRNGType = Ecuyer | Mersenne

newtype PDF = PDF { unPDF :: [(Int, Double)] }
  deriving (Show, Eq)
newtype CDF = CDF { unCDF :: Map Double (Maybe Int) }
  deriving (Show, Eq)

data Engine = Engine{
    cdf    :: CDF
   ,uniformRng :: UniformRNG
  }
