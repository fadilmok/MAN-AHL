{-# LANGUAGE BangPatterns #-}
-- | Module containing all the key Types
module ManAhl.Core.Types(
  -- * Engines
  ProbaUniEngine,
  StatUniEngine,
  ProbaWPEngine,
  StatWPEngine,
  -- * Types
  CDF(..), PDF(..),
  PdfPillars,
  CdfPillars,
  UniformRNGType(..),
  UniformRNG(..),
  EngineParams(..),
  Stats(..)
) where

import Control.Monad.State.Strict
import Control.Monad.Reader
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
-- contains the PDF, CDF
-- needed to compute the weighted probabilities
data EngineParams = EngineParams {
    pdf     :: PDF
   ,cdf     :: !CDF
   ,uniRngT :: Maybe UniformRNGType
  }

-- | Statistic of a given Run [a]
--  hsCount : represent the frequency of each unique element
--  hsTotalCount : the total nb of element
data Stats a = Stats {
    hsCount       :: !(Map a Int)
   ,hsTotalCount  :: !Int
  } deriving Show

-- | Bounded Uniform Probability Engine
type ProbaUniEngine a = State UniformRNG a
-- | Cumulative Statistic Engine for a Uniform distribution
type StatUniEngine = StateT (Stats Double) (State UniformRNG) (Stats Double)

-- | Weighted Probility Engine
type ProbaWPEngine a = ReaderT EngineParams (State UniformRNG) a
-- | Cumulative Statistic Engine for a Weighted distribution
type StatWPEngine = ReaderT EngineParams (StateT (Stats (Maybe Int)) (State UniformRNG)) (Stats (Maybe Int))

