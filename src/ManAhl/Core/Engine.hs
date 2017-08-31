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

-- | Create a weighted probability engine
-- The creation can fail if the pdf pillars are incorrect.
--
mkEngineParams :: PdfPillars -> Maybe UniformRNGType -> Either String EngineParams
mkEngineParams pdfP rT =
  either
      (\ msg -> Left msg)
      (\ p -> Right EngineParams {
        pdf = p
       ,cdf = mkCdf p
       ,uniRngT = rT
     }) $ mkPdf pdfP

runProbaEngine :: EngineParams -> ProbaWPEngine a -> IO a
runProbaEngine p e = do
  uniRng <- mkUniformRNG $ uniRngT p
  return $ flip evalState uniRng $ runReaderT e p

runStatEngine :: EngineParams -> StatWPEngine -> IO (Stats (Maybe Int))
runStatEngine p e = do
  uniRng <- mkUniformRNG $ uniRngT p
  return $ flip evalState uniRng $
    flip evalStateT (Stats Map.empty 0) $
      runReaderT e p

nextNum :: ProbaWPEngine (Maybe Int)
nextNum = do
  EngineParams _ cdf _ <- ask
  uniRng <- get
  let (!x, !r) = runState nextVal uniRng
  put r
  let !y = inverseCdf cdf x
  return y

nextNums :: Int -> ProbaWPEngine [Maybe Int]
nextNums n = replicateM n nextNum

nextStat :: StatWPEngine
nextStat = do
  EngineParams _ cdf _ <- ask
  uniRng <- lift $ lift get
  let (!x, !r) = runState nextVal uniRng
  lift $ lift $ put r
  let !y = inverseCdf cdf x

  Stats !cs !n <- get
  let !stats = Stats {
          hsCount = Map.insertWith (+) y 1 cs
         ,hsTotalCount = n + 1
        }
  put stats
  return stats

allStats :: Int -> StatWPEngine
allStats 0 = error "You need at least one element"
allStats n = foldl' (\ !acc _ -> acc >> nextStat) nextStat [1..n]

