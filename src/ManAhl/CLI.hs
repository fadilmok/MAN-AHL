-- | Module providing a Command Line Input API
module ManAhl.CLI(
  parse,
  run,
  help,
  Query(..), Result(..),
  pillarsDefault
) where

import ManAhl.Core.Types
import ManAhl.Core.UniformEngine
import ManAhl.Core.Analytics
import ManAhl.Core.WeightedEngine

import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Map (toList)

-- | Default pillars if not specified
pillarsDefault :: PdfPillars
pillarsDefault = PdfPillars [(1, 0.2), (2, 0.3), (3, 0.1), (4, 0.15)]

-- | Query to Run from the parsed input,
-- Computing the statistics for :
-- Either a weighted distribution
-- computed from the pillars for nSims and a selected uniform rng
-- can be specified as well.
-- Or a bounded uniform distribution given nSims and
-- a selected uniform rng
data Query =
      RunWeightedWith PdfPillars Int UniformRNGType
    | RunUniformWith Int UniformRNGType
  deriving (Show, Eq)
instance NFData Query where
  rnf (RunWeightedWith (PdfPillars p) n u) = rnf p `seq` rnf n
  rnf (RunUniformWith n u) = rnf n

data Result =
      ResultWeighted WeightedStats
    | ResultUniform UniStats

-- | The run type using either the weighted probability or
-- uniform probability engine
data RunType = Weighted | Uniform
  deriving (Show, Read)

-- | Helper to display the Results nicely
showRes :: Show a => String -> Distribution a -> [(a, Double)] -> Int -> String
showRes name stats proba n = unlines $
     [
      "Result " ++ name ++ " Random Number Engine, " ++ show n ++ " random numbers."
     ,"Probabilities:"
     ] ++ showList' proba
     ++ ["", "Distribution:"]
     ++ showList' (toList $ unPWC $ unDist stats)
  where
    showList' :: (Show a, Show b) => [(a, b)] -> [String]
    showList' = map (\(x, p) -> show x ++ ";" ++ show p )

instance Show Result where
  show (ResultWeighted (Stats stats n (Just proba))) =
    showRes "Weighted" stats proba n
  show (ResultUniform (Stats stats n (Just proba))) =
    showRes "Uniform" stats proba n
  show _ = "Error"

-- | Parse the input argument into a query.
parse :: [String] -> Maybe Query
parse xs = do
    args <- getArgs
    let run     = fromMaybe Weighted $ fmap read $ lookup "run" args
        pillars = fromMaybe pillarsDefault $ fmap (PdfPillars . read) $
                    lookup "pillars" args
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

-- | Run a given query using the appropriate engine
run :: Query -> IO (Either String Result)
run (RunUniformWith nSims rngT) = do
  rng <- mkUniformRNG rngT
  return $ Right $ ResultUniform $
    computeStats UniformEngineParams rng
      (allStats nSims :: StatUniEngine UniStats)
run (RunWeightedWith pdfPillars nSims rngT) = do
  rng <- mkUniformRNG rngT
  let p = mkEngineParams pdfPillars
  return $ case p of
    Left s -> Left s
    Right e -> Right $ ResultWeighted $
      computeStats e rng (allStats nSims :: StatWPEngine WeightedStats)

-- | Give the CLI help
help :: [(String, String)]
help = [
    ("-help","Produce this help.")
   ,("-pillars=[(1,0.2),(2, 0.3),(3, 0.1),(4, 0.15)]", "PDF Pillars")
   ,("-nSims=1000000", "Number of random number generated")
   ,("-rng=Mersenne", "RNG type: Mersenne or Ecuyer")
   ,("-run=Weighted", "Run type: Weighted or Uniform")
  ]
