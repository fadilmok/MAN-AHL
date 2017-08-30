{-# LANGUAGE BangPatterns #-}
-- | Module containing all the key Types
module ManAhl.Core.Types(
  -- * Random Numbers
  Engine(..),
  UniformRNGType(..),
  UniformRNG(..),
  -- * Analytics
  CDF(..), PDF(..),
  PdfPillars,
  CdfPillars,
  Stats(..)
) where

import Control.Monad.State
import Data.Map.Strict
import qualified System.Random as Ecuyer
import qualified System.Random.Mersenne.Pure64 as Mersenne

-- | Uniform Random Number generator,
-- containing Ecuyer and Mersenne Twister rng
data UniformRNG
  = RandomEcuyer   {-# UNPACK #-} !Ecuyer.StdGen
  | RandomMersenne {-# UNPACK #-} !Mersenne.PureMT
  deriving Show

-- | Enum type allowing the selection of Uniform RNG
data UniformRNGType = Ecuyer | Mersenne
  deriving (Show, Read, Eq)

-- | Pillars points of the Probability Density Function
type PdfPillars = [(Int, Double)]
-- | Pillars points of the Cumulative Distribution Function
type CdfPillars = [(Double, Maybe Int)]

-- | Discrete Probabily Density Function,
-- modelised using a TreeMap
newtype PDF = PDF { unPDF :: Map Int Double }
  deriving (Show, Eq)
-- | Discrete Cumulative Distribution Function
-- modelised using a TreeMap
newtype CDF = CDF { unCDF :: Map Double (Maybe Int) }
  deriving (Show, Eq)

-- | Weighted Probability Engine
data Engine = Engine{
    pdf     :: PDF
   ,cdf     :: !CDF
   ,unifRng :: UniformRNG
  }

data Stats a = Stats {
    hsCount       :: Map a Int
   ,hsProba       :: Map a Double
   ,hsTotalCount  :: Int
  } deriving Show
