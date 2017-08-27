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

mkEngine :: PdfPillars -> Maybe UniformRNGType -> IO Engine
mkEngine pdf typ = do
  rng <- mkUniformRNG typ
  let pdf' = mkPdf pdf
  return Engine {
        pdf = pdf'
       ,cdf = mkCdf pdf'
       ,uniformRng = rng
    }

nextNum :: Engine -> (Maybe Int, Engine)
nextNum (Engine p c rng) =
  let (x, r) = next rng in (inverseCdf c x, Engine p c r)

nextNums :: Engine -> Int -> ([Maybe Int], Engine)
nextNums e@(Engine _ _ rng) n = (fst $ unzip rs, e')
  where rs@((_,e'):_) = foldl go [] [1..n]
        go [] _ = [nextNum e]
        go (x@(_,r):xs) _ = nextNum r : x : xs

nextNum' :: Engine -> Maybe Int
nextNum' = fst . nextNum

nextNums' :: Engine -> Int -> [Maybe Int]
nextNums' e n = fst $ nextNums e n

