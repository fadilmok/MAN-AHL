module ManAhl.Core.Types(
  Engine(..),
  UniformRNGType(..),
  UniformRNG(..),
  CDF(..), PDF(..),
  PdfPillars,
  CdfPillars,
  Histogram(..)
) where

import Control.Monad.State
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
    pdf  :: PDF
   ,cdf  :: CDF
   ,seed :: UniformRNG
  }

data Histogram = Histogram {
    hsCount       :: Map (Maybe Int) Int
   ,hsStat        :: Map (Maybe Int) Double
   ,hsTotalCount  :: Int
  } deriving Show
