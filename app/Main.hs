module Main (main) where
import FactoredRandomNumbers
import Data.Time.Clock

import GHC.Environment (getFullArgs)

main :: IO ()
main = do 
  putStrLn "Hello"
  putStrLn "Searching all args..."
  args <- getFullArgs
  print args
  putStrLn "Parallel Strategies"
  x <- timeit (preFactoredNumOfBitSize 92)
  print x 
  putStrLn "Parallel threads/actions/processes"
  y <- timeit (preFactoredNumOfBitSizePar 92)
  print y 


-- | Helper function
timeit :: IO a -> IO (Maybe a, NominalDiffTime)
timeit action = do
  start <- getCurrentTime
  value <- action
  end <- getCurrentTime
  pure (Just value, diffUTCTime end start)
