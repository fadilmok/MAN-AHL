module ManAhl.Run (
  main
) where

import ManAhl.Core.Engine
import ManAhl.Core.Types
import ManAhl.Core.Analytics

main :: IO()
main = do
  let datum = [(1 , 0.2), (3, 0.5), (10, 0.1), (100,0.2) ]
  engine <- mkEngine datum Nothing
  print $ nextNum' engine
  print $ mkHistogram $ nextNums' engine 100000

