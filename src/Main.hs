module Main (
  main
) where

import ManAhl.Core.Engine

main :: IO()
main = do
  let datum = [(1 , 0.2), (3, 0.5), (7, 0.3)]
  engine <- mkEngine datum Nothing
  print $ fst $ nextNum engine
  print $ fst $ nextNums engine 10


