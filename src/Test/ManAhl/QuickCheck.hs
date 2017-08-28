module Test.ManAhl.QuickCheck(
  run,
  nTests
) where

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

nTests :: Int
nTests = 2000

run :: Testable prop => prop -> IO Bool
run p = fmap isSuccess $
  quickCheckWithResult stdArgs{ maxSuccess = nTests } p
