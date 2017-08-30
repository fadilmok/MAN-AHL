module ManAhl.Run (
  main
) where

import ManAhl.CLI
import Control.DeepSeq
import Control.Exception
import System.Environment
import Text.Printf

main :: IO()
main = do
  args <- getArgs
  query <- try $
            evaluate $ force $ parse args

  case query :: Either SomeException (Maybe Query) of
    Right (Just q) -> do
      r <- run q
      case r of
        Left err -> do
          putStrLn "There was an error during the run: "
          putStrLn err
        Right res -> print res
    _ -> do
      putStrLn "Error in the input argurments"
      putStrLn "Follow the help below: "
      mapM_ (\ (x, y) -> printf "%-35s: %s \n" x y) help

