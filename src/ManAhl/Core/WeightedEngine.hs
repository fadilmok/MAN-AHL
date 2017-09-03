{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances #-}
-- | The weighted proba engine
module ManAhl.Core.WeightedEngine(
  -- * Creation
  mkEngineParams,
  -- * Engines
  ProbaWPEngine(),
  StatWPEngine()
) where

import ManAhl.Core.Types
import ManAhl.Core.UniformEngine
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
        Right WeightedEngineParams {
            pdf = p
           ,cdf = cdf'
           ,iCdf = mkInvCdf cdf'
         }) $ mkPdf pdfP

instance ProbaEngine ProbaWPEngine (Maybe Int) where
  computeProba p uniRng e =
    flip evalState uniRng $
      runReaderT (unPWPE e) p

  nextNum = do
    WeightedEngineParams _ _ iCdf <- ask
    uniRng <- get
    let (!x, !r) = runState (unPUIE nextNum) uniRng
    put r
    let !y = invCdf iCdf x
    return y

instance StatEngine StatWPEngine (Maybe Int) where
  computeStats p uniRng e = let
      s = flip evalState
        (Stats defaultDist 0 Nothing, uniRng) $
          runReaderT (unSWP e) p
       in s{ hsProba = Just $ probabilities $ hsDistri s }

  nextStat = do
    WeightedEngineParams _ _ iCdf <- ask
    (Stats cs n _, uniRng) <- get
    let (!x, !r) = runState (unPUIE nextNum) uniRng
        !y = invCdf iCdf x

        !stats = Stats {
            hsDistri      = increaseCount y cs
           ,hsTotalCount  = n + 1
           ,hsProba       = Nothing
          }
    put (stats, r)
    return stats

