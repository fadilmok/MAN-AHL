module ManAhl.Core.Engine(
  -- * Creation
  mkEngine,
  -- * Generation
  nextNum,
  nextNums,
  nextNum',
  nextNums'
) where

import ManAhl.Core.Types
import ManAhl.Core.Random
import ManAhl.Core.Analytics

mkEngine :: [(Int, Double)] -> Maybe UniformRNGType -> IO Engine
mkEngine pdf typ = return . Engine (mkCdf $ PDF pdf) =<< mkUniformRNG typ

nextNum :: Engine -> (Maybe Int, Engine)
nextNum (Engine xs rng) = let (x, r) = next rng in (inverseCdf xs x, Engine xs r)

nextNums :: Engine -> Int -> ([Maybe Int], Engine)
nextNums e@(Engine xs rng) n = (fst $ unzip rs, e')
  where rs@((_,e'):_) = foldl go [] [1..n]
        go [] _ = [nextNum e]
        go (x@(_,r):xs) _ = nextNum r : x : xs

nextNum' :: Engine -> Maybe Int
nextNum' = fst . nextNum

nextNums' :: Engine -> Int -> [Maybe Int]
nextNums' e n = fst $ nextNums e n

