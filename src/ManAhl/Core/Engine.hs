{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
-- | The weighted proba engine
module ManAhl.Core.Engine(
  -- * Creation
  mkEngineParams,
  -- ** Probabilities
  nextNum,
  nextNums,
  -- ** Statistics
  nextStat,
  allStats,
  -- * Execution
  runProbaEngine,
  runStatEngine
) where

import ManAhl.Core.Types
import ManAhl.Core.Random
import ManAhl.Core.Analytics

import Data.List (foldl')
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map as Map

-- | Create a weighted probability engine parameters
-- The creation can fail if the pdf pillars are incorrect.
--
mkEngineParams :: PdfPillars -> Either String EngineParams
mkEngineParams pdfP =
  either
      (\ msg -> Left msg)
      (\ p -> let cdf' = mkCdf p in
        Right EngineParams {
            pdf = p
           ,cdf = cdf'
           ,iCdf = mkInvCdf cdf'
         }) $ mkPdf pdfP

-- | Compute the weighted probabilities
runProbaEngine :: EngineParams -> UniformRNG -> ProbaWPEngine a -> a
runProbaEngine p uniRng e =
  flip evalState uniRng $
    runReaderT e p

-- | Compute the cumulative statistic for the weighted probabilities
runStatEngine :: EngineParams -> UniformRNG -> StatWPEngine -> Stats (Maybe Int)
runStatEngine p uniRng e =
  flip evalState uniRng $
    flip evalStateT (Stats (Distribution $ PieceWiseCurve Map.empty) 0) $
      runReaderT e p

-- | Engine to compute the next weighted probability
nextNum :: ProbaWPEngine (Maybe Int)
nextNum = do
  EngineParams _ _ iCdf <- ask
  uniRng <- get
  let (!x, !r) = runState nextVal uniRng
  put r
  let !y = invCdf iCdf x
  return y

-- | Engine to compute n next weighted probabilties
nextNums :: Int -> ProbaWPEngine [Maybe Int]
nextNums n = replicateM n nextNum

-- | Engine to compute the next cumulative statistics
nextStat :: StatWPEngine
nextStat = do
  EngineParams _ _ iCdf <- ask
  uniRng <- lift $ lift get
  let (!x, !r) = runState nextVal uniRng
  lift $ lift $ put r
  let !y = invCdf iCdf x

  Stats cs n <- get
  let !stats = Stats {
          hsDistri = add cs y
         ,hsTotalCount = n + 1
        }
  put stats
  return stats

-- | Engine to compute the cumulative statistics for n
-- weighted probabilities
-- It fails if n is 0
allStats :: Int -> StatWPEngine
allStats 0 = error "You need at least one element"
allStats n = allStats' (n - 1) nextStat
  where
    allStats' :: Int -> StatWPEngine -> StatWPEngine
    allStats' 0 acc = acc
    allStats' !n acc = allStats' (n - 1) $ acc >> nextStat

