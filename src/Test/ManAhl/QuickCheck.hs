-- | Module providing the tool to run the tets
module Test.ManAhl.QuickCheck(
  -- * Run
  run, runWith, runTest,
  -- * Constants
  nTests, nRand,
  -- * Tools
  failTest, time,
  -- * Generators
  genPdfPillars,
  -- * Type
  TestSuite, Test(..)
) where

import ManAhl.Core.Types
import ManAhl.Core.UniformEngine

import Control.Exception
import Control.DeepSeq
import Control.Monad
import System.CPUTime
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

-- | TestSuite type
type TestSuite = [(String, Test)]
data Test =
    TestQC                    (IO Bool)
  | TestQCRng UniformRNGType  (UniformRNG -> IO Bool)
  | TestPure                  (() -> Bool)
  | TestIO                    (IO Bool)

runTest :: Test -> IO Bool
runTest (TestQC t) = t
runTest (TestQCRng rT t) = do
  r <- mkUniformRNG rT
  t r
runTest (TestPure t) = do
  let res = t()
  putStrLn $ " Test " ++ if res then "Passed" else "Failed"
  return res
runTest (TestIO t) = do
  res <- t
  putStrLn $ " Test " ++ if res then "Passed" else "Failed"
  return res

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
failTest :: a -> IO Bool
failTest f = do
  res' <- try $ do
        evaluate f
        return False

  let res = case (res' :: Either SomeException Bool) of
              Left e -> True
              Right _ -> False
  return res

-- | Time an IO action
time :: NFData t => t -> IO Double
time f = do
  start <- getCPUTime
  x <- evaluate f
  rnf x `seq` return()
  end <- getCPUTime
  return $ fromIntegral (end - start) / 10^12

-- | QuickCheck generator to build pseudo random PDF pillars
-- We ensure that they are correct pillars.
genPdfPillars :: Gen PdfPillars
genPdfPillars = do
    n <- choose (1, 10) :: Gen Int
    suchThat (
        PdfPillars `liftM` snd `liftM`
          foldM (\ (l, xs) _ -> do
              i <- choose (-1000, 1000)
              x <- if l >= 1
                     then return 0
                     else choose (0.001, 1 - l)
              return ( x + l, (i, x):xs ) )
          (0, []) [1 .. n]
      )
      $ \ (PdfPillars xs) -> let s = foldl (\acc (_, x) -> x + acc) 0 xs
                  in s > 0 && s <= 1

