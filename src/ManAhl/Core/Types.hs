{-# LANGUAGE BangPatterns #-}
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
import Data.Map.Strict
import qualified System.Random as Ecuyer
import qualified System.Random.Mersenne.Pure64 as Mersenne

data UniformRNG
  = RandomEcuyer   {-# UNPACK #-} !Ecuyer.StdGen
  | RandomMersenne {-# UNPACK #-} !Mersenne.PureMT
  deriving Show

data UniformRNGType = Ecuyer | Mersenne
  deriving (Show, Read, Eq)

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

data Histogram a = Histogram {
    hsCount       :: Map a Int
   ,hsStat        :: Map a Double
   ,hsTotalCount  :: Int
  } deriving Show
