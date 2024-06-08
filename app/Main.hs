module Main (main) where

import FactoredRandomNumbers

main :: IO ()
main = do 
    putStrLn "Hello"
    x <- preFactoredNumOfBitSizePar 62
    case x of 
        Left _ -> putStrLn "some went wrong"
        Right u -> print u 