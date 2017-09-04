-- | Module providing a Command Line Input API
module ManAhl.CLI(
  parse,
  run,
  help,
  Query(..), Result(..),
) where

import ManAhl.Core.Types
import ManAhl.Core.UniformEngine
import ManAhl.Core.Analytics
import ManAhl.Core.WeightedEngine

import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Map (toList) -- | Query to Run from the parsed input, -- Computing the statistics for :
-- Either a weighted distribution
-- computed from the pillars for nSims and a selected uniform rng
-- can be specified as well.
-- Or a bounded uniform distribution given nSims and
-- a selected uniform rng
data Query =
      RunWeightedWith PdfPillars Int UniformRNGType
    | RunUniformWith [(Double, Double)] Int UniformRNGType
  deriving (Show, Eq)
instance NFData Query where
  rnf (RunWeightedWith (PdfPillars p) n u) = rnf p `seq` rnf n
  rnf (RunUniformWith p n u) = rnf n `seq` rnf p

data Result =
      ResultWeighted WeightedStats
    | ResultUniform UniStats

-- | The run type using either the weighted probability or
-- uniform probability engine
data RunType = Weighted | Uniform
  deriving (Show, Read)

-- | Helper to display the Results nicely
showRes :: Show a => String -> Stats a -> String
showRes name (Stats dist n (Just proba)
    (Just diffProba) (Just diffMean) (Just diffStd)
    (Just diffHi) (Just diffLow) ) = unlines $
     [
      "Result " ++ name ++ " Random Number Engine, " ++ show n ++ " random numbers."
     ,"Probabilities:"
     ] ++ showList' proba
     ++ ["", "Distribution:"]
     ++ showList' (toPillars dist)
     ++ ["", "Diff res and input PDFs:"]
     ++ showList' diffProba
     ++ ["", "Mean of the PDF Diffs; " ++ show diffMean]
     ++ ["StdDev of the PDF Diffs; " ++ show diffStd]
     ++ ["Highest of the PDF Diffs; " ++ show diffHi]
     ++ ["Lowest of the PDF Diffs; " ++ show diffLow]
  where
    showList' :: (Show a, Show b) => [(a, b)] -> [String]
    showList' = map (\(x, p) -> show x ++ ";" ++ show p )

instance Show Result where
  show (ResultWeighted stats) =
    showRes "Weighted" stats
  show (ResultUniform stats) =
    showRes "Uniform" stats

-- | Parse the input argument into a query.
parse :: [String] -> Maybe Query
parse xs = do
    args <- getArgs
    let run     = fromMaybe Weighted $ fmap read $ lookup "run" args
        pillars = lookup "pillars" args
        nSims   = fromMaybe 1000000 $ fmap read $ lookup "nSims" args
        rng     = fromMaybe Mersenne $ fmap read $ lookup "rng" args
    return $ case run of
      Weighted -> RunWeightedWith
            (maybe pillarsWDefault PdfPillars $ fmap read pillars) nSims rng
      Uniform -> RunUniformWith
            (fromMaybe pillarsUDefault $ fmap read pillars) nSims rng
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

-- | Run a given query using the appropriate engine
run :: Query -> IO (Either String Result)
run (RunUniformWith pdfPillars nSims rngT) = do
  let p = mkUPEngineParams pdfPillars
  case p of
    Left s -> return $ Left s
    Right e -> do
      rng <- mkUniformRNG rngT
      return $ Right $ ResultUniform $
          computeStats e rng
            (allStats nSims :: StatUniEngine UniStats)
run (RunWeightedWith pdfPillars nSims rngT) = do
  let p = mkWPEngineParams pdfPillars
  case p of
    Left s -> return $ Left s
    Right e -> do
      rng <- mkUniformRNG rngT
      return $ Right $ ResultWeighted $
          computeStats e rng (allStats nSims :: StatWPEngine WeightedStats)

-- | Give the CLI help
help :: [(String, String)]
help = [
    ("-help","Produce this help.")
   ,("-pillars='[(1,0.2),(2, 0.3),(3, 0.1),(4, 0.15)]'", "Input the PDF Pillars")
   ,("-nSims=1000000", "Number of random number generated")
   ,("-rng=Mersenne", "RNG type: Mersenne or Ecuyer")
   ,("-run=Weighted", "Run type: Weighted or Uniform")
  ]
