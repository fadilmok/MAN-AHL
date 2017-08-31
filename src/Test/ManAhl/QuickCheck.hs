-- | Module providing the tool to run the tets
module Test.ManAhl.QuickCheck(
  -- * Run
  run, runWith,
  -- * Constants
  nTests, nRand,
  -- * Tools
  failTest, time,
  -- * Generators
  genPdfPillars
) where

import ManAhl.Core.Types

import Control.Exception
import Control.DeepSeq
import Control.Monad
import System.CPUTime
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

-- | Number of quickcheck tests
nTests :: Int
nTests = 1000

-- | Number of random number to simulate in some tests
nRand :: Int
nRand = 100000

-- | Run a quickcheck test nTests times
run :: Testable prop => prop -> IO Bool
run = runWith nTests

-- | Run a quickcheck test n times
runWith :: Testable prop => Int -> prop -> IO Bool
runWith n p = fmap isSuccess $
  quickCheckWithResult stdArgs{ maxSuccess = n } p

-- | Test if the input function raises an exception
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

-- | Time an IO action
time :: NFData t => IO t -> IO Double
time f = do
  start <- getCPUTime
  x <- f
  rnf x `seq` return()
  end <- getCPUTime
  return $ fromIntegral (end - start) / 10^12

genPdfPillars :: Gen PdfPillars
genPdfPillars = do
    n <- choose (1, 10) :: Gen Int
    suchThat (
        snd `liftM`
          foldM (\ (l, xs) _ -> do
              i <- choose (-1000, 1000)
              x <- if l >= 1
                     then return 0
                     else choose (0.001, 1 - l)
              return ( x + l, (i, x):xs ) )
          (0, []) [1 .. n]
      )
      $ \xs -> let s = foldl (\acc (_, x) -> x + acc) 0 xs
                  in s > 0 && s <= 1

