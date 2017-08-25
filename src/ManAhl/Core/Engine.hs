module ManAhl.Core.Engine(
  -- * Creation
  mkEngine,
  -- * Generation
  nextNum,
  nextNums
) where

import ManAhl.Core.Types
import ManAhl.Core.Random

mkEngine :: [(Int, Double)] -> Maybe UniformRNGType -> IO Engine
mkEngine xs typ = return . Engine xs =<< mkUniformRNG typ

nextNum :: Engine -> (Double, Engine)
nextNum = undefined

nextNums :: Engine -> Int -> ([Double], Engine)
nextNums = undefined

