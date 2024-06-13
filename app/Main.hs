module Main (main) where

import Data.Time.Clock
import FactoredRandomNumbers
import GHC.Environment (getFullArgs)

main :: IO ()
main = do
  putStrLn "Hello"
  putStrLn "Searching all args..."
  args <- getFullArgs
  print args
  let sizeExponent = 92 :: Integer
  putStrLn "               "
  putStrLn "Parallel --- BitSize Range 90"
  x <- timeit (preFactoredNumOfBitSize sizeExponent)
  print x
  putStrLn "Parallel-Concurrent -- Bitsize Range 90"
  y <- timeit (preFactoredNumOfBitSizePar sizeExponent)
  print y
  putStrLn "Ratio of Parallel - Concurrent / Parallel "
  print $ snd y / snd x
  putStrLn "------------"

  putStrLn "Parallel --- BitSize Range 45"
  x1 <- timeit (preFactoredNumOfBitSize $ sizeExponent `div` 2)
  print x1
  putStrLn "Parallel-Concurrent -- Bitsize Range 45"
  y1 <- timeit (preFactoredNumOfBitSizePar $ sizeExponent `div` 2)
  print y1
  putStrLn "Ratio of Parallel - Concurrent / Parallel "
  print $ snd y1 / snd x1
  putStrLn "------------"

  putStrLn "Parallel --- BitSize Range 22"
  x2 <- timeit (preFactoredNumOfBitSize $ sizeExponent `div` 4)
  print x2
  putStrLn "Parallel-Concurrent -- Bitsize Range 22"
  y2 <- timeit (preFactoredNumOfBitSizePar $ sizeExponent `div` 4)
  print y2
  putStrLn "Ratio of Parallel - Concurrent / Parallel "
  print $ snd y2 / snd x2
  putStrLn "------------"

-- | Helper function
timeit :: IO a -> IO (Maybe a, NominalDiffTime)
timeit action = do
  start <- getCurrentTime
  value <- action
  end <- getCurrentTime
  pure (Just value, diffUTCTime end start)
