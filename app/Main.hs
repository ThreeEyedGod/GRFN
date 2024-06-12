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
  let sizeExponent = 92 :: Integer
  putStrLn "Parallel Strategies--90+"
  x <- timeit (preFactoredNumOfBitSize sizeExponent)
  print x 
  putStrLn "Plus Parallel threads/actions/processes-90+"
  y <- timeit (preFactoredNumOfBitSizePar sizeExponent)
  print y 
  putStrLn "Numer of times 90 Hyper-Parallel slower than Reg " 
  print $ snd y / snd x
  putStrLn "------------"

  putStrLn "Parallel Strategies--45"
  x1 <- timeit (preFactoredNumOfBitSize $ sizeExponent `div` 2)
  print x1 
  putStrLn "Plus Parallel threads/actions/processes-45"
  y1 <- timeit (preFactoredNumOfBitSizePar $ sizeExponent `div` 2)
  print y1 
  putStrLn "Number of times 45 Hyper-Parallel slower  than Reg"
  print $ snd y1 / snd x1
  putStrLn "------------"

  putStrLn "Parallel Strategies--20"
  x2 <- timeit (preFactoredNumOfBitSize $ sizeExponent `div` 4)
  print x2 
  putStrLn "Plus Parallel threads/actions/processes-20"
  y2 <- timeit (preFactoredNumOfBitSizePar $ sizeExponent `div` 4)
  print y2
  putStrLn "Nunmber of times 20 Hyper-Parallel slower than Reg by"
  print $ snd y2 / snd x2
  putStrLn "------------"


-- | Helper function
timeit :: IO a -> IO (Maybe a, NominalDiffTime)
timeit action = do
  start <- getCurrentTime
  value <- action
  end <- getCurrentTime
  pure (Just value, diffUTCTime end start)
