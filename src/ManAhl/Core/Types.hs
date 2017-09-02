{-# LANGUAGE BangPatterns, FlexibleInstances #-}
-- | Module containing all the key Types
module ManAhl.Core.Types(
  -- * Engines
  ProbaUniEngine,
  StatUniEngine,
  ProbaWPEngine,
  StatWPEngine,
  -- * Types
  PieceWiseCurve(..), PdfPillars(..),
  CDF(..), PDF(..), InvCDF(..),
  Distribution(..),
  UniformRNGType(..),
  UniformRNG(..),
  EngineParams(..),
  Stats(..)
) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Map.Strict as Map
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

-- | PDF Pillars
newtype PdfPillars = PdfPillars [(Int, Double)]
  deriving (Show, Eq)
-- | Discrete PieceWise Curve
-- modelised using a TreeMap
newtype PieceWiseCurve a b = PieceWiseCurve { unPWC :: Map a b }
  deriving Show
-- | Discrete Probabily Density Function,
newtype PDF = PDF { unPDF :: PieceWiseCurve (Maybe Int) Double }
  deriving (Show, Eq)
-- | Discrete Cumulative Distribution Function
newtype CDF = CDF { unCDF :: PieceWiseCurve (Maybe Int) Double}
  deriving (Show, Eq)
-- | Discrete Inverse Cumulative Distribution Function
newtype InvCDF = InvCDF { unICDF :: PieceWiseCurve Double (Maybe Int) }
  deriving (Show, Eq)
-- | Distribution
newtype Distribution a = Distribution { unDist :: PieceWiseCurve a Int }
  deriving (Show, Eq)

-- | Weighted Probability Engine Params
-- contains the PDF, CDF, inverseCDF
-- needed to compute the weighted probabilities
data EngineParams = EngineParams {
    pdf     :: PDF
   ,cdf     :: CDF
   ,iCdf    :: !InvCDF
  }

-- | Statistic of a given Run [a]
--  hsCount : represent the distribution of the  elements
--  hsTotalCount : the total nb of element
data Stats a = Stats {
    hsDistri      :: !(Distribution a)
   ,hsTotalCount  :: !Int
  } deriving Show

-- | Bounded Uniform Probability Engine
type ProbaUniEngine a = State UniformRNG a
-- | Cumulative Statistic Engine for a Uniform distribution
type StatUniEngine = StateT (Stats Double) (State UniformRNG) (Stats Double)

-- | Weighted Probility Engine
type ProbaWPEngine a = ReaderT EngineParams (State UniformRNG) a
-- | Cumulative Statistic Engine for a Weighted distribution
type StatWPEngine = ReaderT EngineParams (StateT (Stats (Maybe Int))
                      (State UniformRNG)) (Stats (Maybe Int))


instance Functor (PieceWiseCurve a) where
  fmap f (PieceWiseCurve m) = PieceWiseCurve $ Map.map f m

instance {-# OVERLAPS #-} Eq (PieceWiseCurve (Maybe Int) Double) where
  PieceWiseCurve lhs == PieceWiseCurve rhs =
    Map.foldl (\acc x -> if not acc then False else x < 0.0001) True $
      Map.unionWith (-) lhs rhs

instance {-# OVERLAPS #-} Eq (PieceWiseCurve Double (Maybe Int)) where
  PieceWiseCurve lhs == PieceWiseCurve rhs =
    PieceWiseCurve (transpose lhs) == PieceWiseCurve (transpose rhs)
      where
        transpose = Map.fromList . Prelude.map (\(x, y) -> (y, x)) . Map.toList

instance {-# OVERLAPPABLE #-} (Eq a, Eq b) => Eq (PieceWiseCurve a b) where
  PieceWiseCurve lhs == PieceWiseCurve rhs = lhs == rhs
