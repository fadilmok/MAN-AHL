module Test.ManAhl.QuickCheck(
  run, runWith,
  nTests, nRand,
  failTest, time
) where

import Control.Exception (evaluate)
import Control.DeepSeq
import System.CPUTime
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Control.Exception

nTests :: Int
nTests = 1000

nRand :: Int
nRand = 100000

run :: Testable prop => prop -> IO Bool
run = runWith nTests

runWith :: Testable prop => Int -> prop -> IO Bool
runWith n p = fmap isSuccess $
  quickCheckWithResult stdArgs{ maxSuccess = n } p

failTest :: IO() -> IO Bool
failTest f = do
  res' <- try $ do
        f
        return False

  let res = case (res' :: Either SomeException Bool) of
              Left e -> True
              Right _ -> False
  putStrLn $ "Test " ++ if res then "Passed" else "FAILED"
  return res

time :: NFData t => IO t -> IO Double
time f = do
  start <- getCPUTime
  x <- f
  rnf x `seq` return()
  end <- getCPUTime
  return $ fromIntegral (end - start) / 10^12

