module Test.ManAhl.QuickCheck(
  run
) where

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

run :: Testable prop => prop -> IO Bool
run p = fmap isSuccess $
  quickCheckWithResult stdArgs{ maxSuccess = 1000 } p
