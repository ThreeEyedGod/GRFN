module Main (main) where

import FactoredRandomNumbers
-- NOTE: must be Control.Monad.Par.Class not Control.Monad.Par
import Control.Concurrent (threadDelay)
-- NOTE: must be Control.Monad.Par.Class not Control.Monad.Par

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Par.Class (fork, get, new, put, spawn)
import Control.Monad.Par.IO (IVar, ParIO, runParIO)
import GHC.Conc (getNumCapabilities)

-- main :: IO ()
-- main = do 
--     putStrLn "Hello"
--     x <- preFactoredNumOfBitSizePar 62
--     case x of 
--         Left _ -> putStrLn "some went wrong"
--         Right u -> print u 


expensiveIOComputation :: Int -> IO Int
expensiveIOComputation x = do
  putStrLn "expensiveIOComputation called"
  threadDelay 2000000
  pure $ x + 1

main :: IO ()
main = do
  x <- runParIO parMain
  print x

parMain :: ParIO [Int]
parMain = do
  let liftedComputations = map (liftIO . expensiveIOComputation) [1, 2, 3, 4]
  runningComputations <- mapM spawn liftedComputations
  mapM get runningComputations

    -- spawn p = do
    -- r <- new
    -- fork (p >>= put r)
    -- return r
