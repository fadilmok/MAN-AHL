{-# LANGUAGE BangPatterns, FlexibleInstances,
          GeneralizedNewtypeDeriving, MultiParamTypeClasses,
          FunctionalDependencies, FlexibleContexts #-}
-- | Module containing all the key Types
module ManAhl.Core.Types(
  -- * Engines
  ProbaEngine(..), StatEngine(..),
  ProbaUniEngine(..),
  StatUniEngine(..),
  ProbaWPEngine(..),
  StatWPEngine(..),
  -- * Types
  PieceWiseCurve(..), PdfPillars(..),
  CDF(..), PDF(..), InvCDF(..),
  Distribution(..),
  UniformRNGType(..),
  UniformRNG(..),
  EngineParams(..),
  Stats(..), UniStats, WeightedStats,
  Distri(..),
  Curve(..)
) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Map.Strict as Map
import System.Random as Ecuyer
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
  deriving (Show, Eq, Curve (Maybe Int) Double)
-- | Discrete Cumulative Distribution Function
newtype CDF = CDF { unCDF :: PieceWiseCurve (Maybe Int) Double}
  deriving (Show, Eq, Curve (Maybe Int) Double)
-- | Discrete Inverse Cumulative Distribution Function
newtype InvCDF = InvCDF { unICDF :: PieceWiseCurve Double (Maybe Int) }
  deriving (Show, Eq, Curve Double (Maybe Int))
-- | Distribution
newtype Distribution a = Distribution { unDist :: PieceWiseCurve a Int }
  deriving (Show, Eq, Curve a Int, NFData)

class Curve b c a | a -> b, a -> c where
  emptyCurve :: a
  -- | O(1) Nb of Pillars
  nPillars :: a -> Int
  -- | O(n log n) create a curve from pillars
  fromPillars :: Ord b => [(b, c)] -> a
  -- | O(n) return the pillars from a curve
  toPillars :: a -> [(b, c)]
  -- | O(1) create a curve from a map
  fromRaw :: Map b c -> a
  -- | O(1) get the inner map
  toRaw :: a -> Map b c
  -- | O(log n) get the values associated to a key
  -- it is unsafe throws an exception if it is out of bounds.
  (!!!) :: (Show b, Ord b) => a -> b -> (b, c)
  -- | O(log n) add a pillar with a combining function
  addWith :: Ord b => (c -> c -> c) -> b -> c -> a -> a
  -- | O(log n) add a pillar
  add :: Ord b => b -> c -> a -> a
  -- | O(n) add a pillar with a combining function
  cFoldl :: (d -> c -> d) -> d -> a -> d
  -- | O(n) appy a function to each pillar
  cMap :: (c -> c) -> a -> a

-- | Class to operate a Distribution
class Distri a where
  -- | O(log n) add an element
  increaseCount :: a -> Distribution a -> Distribution a
  -- | Range used to compute the statistics
  defaultDist :: Distribution a

-- | Weighted Probability Engine Params
-- contains the PDF, CDF, inverseCDF
-- needed to compute the weighted probabilities
data EngineParams = EngineParams {
    pdf     :: PDF
   ,cdf     :: CDF
   ,iCdf    :: !InvCDF
  }

-- | Statistic of a given Run [a]
--  hsDistri : represent the distribution of the  elements
--  hsTotalCount : the total nb of element
data Stats a =
  Stats {
    hsDistri      :: !(Distribution a)
   ,hsTotalCount  :: !Int
   ,hsProba       :: Maybe [(a, Double)]
  }
type UniStats = Stats Double
type WeightedStats = Stats (Maybe Int)

-- | Class to compute probabilities given an engine.
class Monad a => ProbaEngine a b | a -> b where
  -- | Compute the probabilities
  computeProba :: Maybe EngineParams -> UniformRNG -> a c -> c
  -- | Prepare the next random number
  nextNum :: a b
  -- | Prepare the n next random numbers
  nextNums :: Int -> a [b]
  nextNums n = replicateM n nextNum

class Monad a => StatEngine a b | a -> b where

  computeStats :: Maybe EngineParams -> UniformRNG -> a (Stats b) -> (Stats b)

  nextStat :: a (Stats b)

  allStats :: Int -> a (Stats b)
  allStats 0 = error "You need at least one element"
  allStats n = allStats' (n - 1) nextStat
    where
      allStats' 0 acc = acc
      allStats' !n acc = allStats' (n - 1) $ acc >> nextStat

-- | Bounded Uniform Probability Engine
newtype ProbaUniEngine a = ProbaUniEngine { unPUIE :: State UniformRNG a }
  deriving (Functor, Applicative, Monad, MonadState UniformRNG)

-- | Cumulative Statistic Engine for a Uniform distribution
newtype StatUniEngine a = StatUniEngine {
    unSUE :: State (UniStats, UniformRNG) a  }
  deriving (Functor, Applicative, Monad, MonadState (UniStats, UniformRNG))

-- | Weighted Probility Engine
newtype ProbaWPEngine a = ProbaWPEngine {
     unPWPE :: ReaderT EngineParams (State UniformRNG) a
   } deriving (
      Functor, Applicative, Monad, MonadState UniformRNG, MonadReader EngineParams)

-- | Cumulative Statistic Engine for a Weighted distribution
newtype StatWPEngine a = StatWPEngine {
    unSWP :: ReaderT EngineParams (State (WeightedStats, UniformRNG)) a
  } deriving (
      Functor, Applicative, Monad, MonadState (WeightedStats, UniformRNG),
      MonadReader EngineParams)

instance Functor (PieceWiseCurve a) where
  fmap f (PieceWiseCurve m) = PieceWiseCurve $ Map.map f m

instance {-# OVERLAPS #-} Eq (PieceWiseCurve (Maybe Int) Double) where
  PieceWiseCurve lhs == PieceWiseCurve rhs =
    Map.foldl (\acc x -> if not acc then False else x < 0.001) True $
      Map.unionWith (-) lhs rhs

instance {-# OVERLAPS #-} Eq (PieceWiseCurve Double (Maybe Int)) where
  PieceWiseCurve lhs == PieceWiseCurve rhs =
    PieceWiseCurve (transpose lhs) == PieceWiseCurve (transpose rhs)
      where
        transpose = Map.fromList . Prelude.map (\(x, y) -> (y, x)) . Map.toList

instance {-# OVERLAPPABLE #-} (Eq a, Eq b) => Eq (PieceWiseCurve a b) where
  PieceWiseCurve lhs == PieceWiseCurve rhs = lhs == rhs

instance Curve a b (PieceWiseCurve a b) where
  emptyCurve = PieceWiseCurve Map.empty
  nPillars = Map.size . toRaw
  fromPillars = fromRaw . Map.fromList
  toPillars = Map.toList . toRaw
  fromRaw = PieceWiseCurve
  toRaw = unPWC
  PieceWiseCurve m !!! x =
    case x `Map.lookupGE` m of
      Just r -> r
      Nothing -> error $ "Fails: " ++ show x
  addWith f x y c = PieceWiseCurve $ Map.insertWith f x y $ toRaw c
  add x y c = PieceWiseCurve $ Map.insert x y $ toRaw c
  cFoldl f x m = Map.foldl f x $ toRaw m
  cMap = fmap

instance Distri (Maybe Int) where
  increaseCount x = addWith (+) x 1
  defaultDist = emptyCurve
instance Distri Double where
  increaseCount x = addWith (+) (fst $ defaultDist !!! x) 1
  defaultDist = fromPillars $ zip (Prelude.map (/100) [1, 2 .. 100]) [0..]

instance NFData a => NFData (Stats a) where
  rnf (Stats d t p) = rnf d `seq` rnf t `seq` rnf p
instance (NFData a, NFData b) => NFData (PieceWiseCurve a b) where
  rnf (PieceWiseCurve c) = rnf c
