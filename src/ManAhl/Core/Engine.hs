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

import Control.Monad.State

mkEngine :: PdfPillars -> Maybe UniformRNGType -> IO (Either String Engine)
mkEngine pdf typ = do
  rng <- mkUniformRNG typ
  let pdf' = mkPdf pdf
  return $
    either
      (\ msg -> Left msg)
      (\ p -> Right Engine {
        pdf = p
       ,cdf = mkCdf p
       ,unifRng = rng
      }) pdf'

nextNum :: State Engine (Maybe Int)
nextNum = do
  Engine pdf cdf seed <- get
  let (x, r) = runState next' seed
  put $ Engine pdf cdf r
  return $ inverseCdf cdf x

nextNums :: Int -> State Engine [Maybe Int]
nextNums n = do
  Engine pdf cdf seed <- get
  let (xs, r) = runState (nexts' n) seed
  put $ Engine pdf cdf r
  return $
    map (inverseCdf cdf) xs

nextNum' :: Engine -> Maybe Int
nextNum' e = evalState nextNum e

nextNums' :: Engine -> Int -> [Maybe Int]
nextNums' e n = evalState (nextNums n) e

