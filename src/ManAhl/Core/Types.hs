module ManAhl.Core.Types(
  Engine(..),
  UniformRNGType(..),
  UniformRNG(..),
  CDF(..), PDF(..),
  PdfPillars,
  CdfPillars,
  Histogram(..)
) where

import Data.Map
import qualified System.Random as Ecuyer
import qualified System.Random.Mersenne.Pure64 as Mersenne

data UniformRNG
  = RandomEcuyer Ecuyer.StdGen
  | RandomMersenne Mersenne.PureMT

data UniformRNGType = Ecuyer | Mersenne

type PdfPillars = [(Int, Double)]
type CdfPillars = [(Double, Maybe Int)]

newtype PDF = PDF { unPDF :: Map Int Double }
  deriving (Show, Eq)
newtype CDF = CDF { unCDF :: Map Double (Maybe Int) }
  deriving (Show, Eq)

data Engine = Engine{
    cdf    :: CDF
   ,uniformRng :: UniformRNG
  }

data Histogram = Histogram {
    hsRaw :: [(Maybe Int, Int)]
   ,hsStat :: [(Maybe Int, Double)]
   ,hsTotalCount :: Int
  } deriving Show
