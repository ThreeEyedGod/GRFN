import Test.Tasty.Bench 
import FactoredRandomNumbers 

main :: IO ()
main =
  defaultMain
    [ bgroup
        "PreFactored Uniform Random Numbers"
        [ 
          -- bench "2^62-parallel " $ whnfIO (preFactoredNumOfBitSizePar 62),
          -- bench "2^62-Non-Parallel " $ whnfIO (preFactoredNumOfBitSize 62),
          -- bench "Regular-500 " $ whnfIO (genARandomPreFactoredNumberLTEn 500)
          bench "2^6-parallel " $ whnfIO (preFactoredNumOfBitSizePar 6),
          bench "2^6-Non-Parallel " $ whnfIO (preFactoredNumOfBitSize 6),
          bench "Regular-50 " $ whnfIO (genARandomPreFactoredNumberLTEn 50)
        ]
    ]
