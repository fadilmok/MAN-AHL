module Test.ManAhl.QuickCheck(
  run, runWith,
  nTests, nRand
) where

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

nTests :: Int
nTests = 1000

nRand :: Int
nRand = 100000

run :: Testable prop => prop -> IO Bool
run = runWith nTests

runWith :: Testable prop => Int -> prop -> IO Bool
runWith n p = fmap isSuccess $
  quickCheckWithResult stdArgs{ maxSuccess = n } p
