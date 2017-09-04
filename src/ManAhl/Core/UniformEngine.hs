{-# LANGUAGE BangPatterns, MultiParamTypeClasses  #-}
module ManAhl.Core.UniformEngine(
  -- * Creation
  mkUniformRNG,
  mkUPEngineParams,
  pillarsUDefault,
  -- * Engines
  ProbaUniEngine(),
  StatUniEngine()
) where

import ManAhl.Core.Types
import ManAhl.Core.Analytics

import Control.Monad
import Control.Monad.State.Strict
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random
import System.Random.Mersenne.Pure64

-- | Make UniformRNG instance of RandGen by delegating the
-- work to the inner RNG
instance RandomGen UniformRNG where
  next (RandomEcuyer rng)       = let (x, r) = next rng in (x, RandomEcuyer r)
  next (RandomMersenne rng)     = let (x, r) = next rng in (x, RandomMersenne r)
  genRange (RandomEcuyer rng)   = genRange rng
  genRange (RandomMersenne rng) = genRange rng
  split (RandomEcuyer rng)      = let (g1, g2) = split rng in
                                   (RandomEcuyer g1, RandomEcuyer g1)
  split (RandomMersenne rng)    = let (g1, g2) = split rng in
                                   (RandomMersenne g1, RandomMersenne g2)

pillarsUDefault :: [(Double, Double)]
pillarsUDefault = tail $ map (\ x-> (x/100, 1/50)) [0,2..100]

-- Create Uniform RNG encapsulating Mersenne or Ecuyer
mkUniformRNG :: UniformRNGType -> IO UniformRNG
mkUniformRNG Ecuyer   = return . RandomEcuyer =<< newStdGen
mkUniformRNG Mersenne = return . RandomMersenne =<< newPureMT

mkUPEngineParams :: [(Double, Double)] -> Either String UEngineParams
mkUPEngineParams pdfP
  | null pdfP = Left "The pdf pillars are empty."
  | foldl (\ acc (x, _) -> if acc then acc else x < 0 || x > 1) False pdfP =
      Left "PDF Pillars contain negative values or greater than 1."
  | foldl (\ acc (_, x) -> if acc then acc else x < 0 || x > 1) False pdfP =
      Left "PDF Pillars contain negative values or greater than 1."
  | any (\(_, x) -> abs( x - snd (head pdfP)) > 0.0001) pdfP =
        Left "All the pdf probabilities must be equal, as it is uniform"
  | abs (foldl (\ acc (_, x) -> acc + x) 0 pdfP - 1 ) > 0.0001 =
        Left "The sum of PDF probabilities are different than 1."
  | otherwise = Right $ UEngineParams $ fromPillars pdfP

instance ProbaEngine ProbaUniEngine Double UEngineParams where
  computeProba _ r e = evalState (unPUIE e) r

  nextNum = state $! randomR (0, 1)

instance StatEngine StatUniEngine Double UEngineParams where
  computeStats p r e = let
      d = fromRaw $ Map.map (const 0::Double -> Int) $ toRaw $ uepPdf p
   in statistics (uepPdf p) $
     evalState (unSUE e) (Stats d 0 Nothing Nothing Nothing Nothing, r)

  nextStat = do
    (stats, uniRng) <- get
    let (!x, !r) = runState (unPUIE nextNum) uniRng
        !stats' = stats {
            hsDistri = addWith (+) (fst $ (hsDistri stats) !!! x) 1 $ hsDistri stats
           ,hsCount  = hsCount stats + 1
          }
    put (stats', r)
    return stats

