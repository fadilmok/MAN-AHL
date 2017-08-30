module ManAhl.CLI(
  parse,
  run,
  help,
  Query(..), Result,
  pillarsDefault
) where

import ManAhl.Core.Types
import ManAhl.Core.Random
import ManAhl.Core.Analytics
import ManAhl.Core.Engine

import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Map (toList)

pillarsDefault :: PdfPillars
pillarsDefault = [(1, 0.2), (2, 0.3), (3, 0.1), (4, 0.15)]

data Query =
      RunWeightedWith PdfPillars Int UniformRNGType
    | RunUniformWith Int UniformRNGType
  deriving (Show, Eq)
instance NFData Query where
  rnf (RunWeightedWith p n u) = rnf p `seq` rnf n
  rnf (RunUniformWith n u) = rnf n

data RunType = Weighted | Uniform
  deriving (Show, Read)

data Result =
   ResultWeighted [(Maybe Int, Double)]
  |ResultUniform  [(Double, Double)]
  deriving Show

parse :: [String] -> Maybe Query
parse xs = do
    args <- getArgs
    let run     = fromMaybe Weighted $ fmap read $ lookup "run" args
        pillars = fromMaybe pillarsDefault $ fmap read $ lookup "pillars" args
        nSims   = fromMaybe 1000000 $ fmap read $ lookup "nSims" args
        rng     = fromMaybe Mersenne $ fmap read $ lookup "rng" args
    return $ case run of
      Weighted -> RunWeightedWith pillars nSims rng
      Uniform -> RunUniformWith nSims rng
  where
    getArgs :: Maybe [(String, String)]
    getArgs = do
      ys <- mapM getArgVal xs
      let args = ["pillars","nSims","rng", "run"]
      if all (==True) $ map (\ (x, _) -> x `elem` args) ys
         then Just ys
         else Nothing
    getArgVal :: String -> Maybe  (String, String)
    getArgVal (x:xs) = if x == '-'
         then Just $ let (n, v) = break (== '=') xs in (n, tail v)
        else Nothing


run :: Query -> IO (Either String Result)
run (RunUniformWith nSims rngT) = do
  rng <- mkUniformRNG $ Just rngT
  let vals = nextVals rng nSims
      stats = mkHistogramUniform vals
  return $
    Right $ ResultUniform $ toList $ hsStat stats
run (RunWeightedWith pdfPillars nSims rngT) = do
  engine <- mkEngine pdfPillars $ Just rngT
  return $ case engine of
    Left s -> Left s
    Right e -> let
        vals = nextNums' e nSims
        res = toList $ hsStat $ mkHistogram vals
      in Right $ ResultWeighted res

help :: [(String, String)]
help = [
    ("-pillars=[(1,0.2),(2, 0.3),(3, 0.1),(4, 0.15)]", "PDF Pillars")
   ,("-nSims=1000000", "Nb of sims")
   ,("-rng=Mersenne", "RNG type: Mersenne or Ecuyer")
   ,("-run=Weighted", "Run type: Weighted or Uniform")
  ]
