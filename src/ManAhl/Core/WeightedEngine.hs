{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances #-}
-- | The weighted proba engine
module ManAhl.Core.WeightedEngine(
  -- * Creation
  mkWPEngineParams,
  pillarsWDefault,
  -- * Engines
  ProbaWPEngine()
) where

import ManAhl.Core.Types
import ManAhl.Core.UniformEngine
import ManAhl.Core.Analytics

import Data.List (foldl')
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map as Map

-- | Default pillars if not specified
pillarsWDefault :: WPdfPillars
pillarsWDefault = WPdfPillars [(1, 0.2), (2, 0.3), (3, 0.1), (4, 0.15)]

-- | Create a weighted probability engine parameters
-- The creation can fail if the pdf pillars are incorrect.
--
mkWPEngineParams :: WPdfPillars -> Either String WEngineParams
mkWPEngineParams pdfP =
  either
      (\ msg -> Left msg)
      (\ p -> let cdf' = mkCdf p in
        Right WEngineParams {
            pdf = p
           ,cdf = cdf'
           ,iCdf = mkInvCdf cdf'
         }) $ mkPdf pdfP

instance ProbaEngine ProbaWPEngine (Maybe Int) WEngineParams where
  computeProba p uniRng e =
    flip evalState uniRng $
      runReaderT (unPWPE e) p

  runProba p r e =
    let (!x, !rng) = flip runState r $
                      runReaderT (unPWPE e) p
    in (x, rng)

  nextNum = do
    WEngineParams _ _ iCdf <- ask
    uniRng <- get
    let (!x, !r) = runState (unPUIE nextNum) uniRng
    put r
    let !y = invCdf iCdf x
    return y

  getPDF _ p = pdf p

