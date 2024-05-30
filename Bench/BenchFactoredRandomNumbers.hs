import Test.Tasty.Bench
import FactoredRandomNumbers

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Fibonacci numbers"
        [ 
          bench "2^62-parallel" $ whnf preFactoredNumOfBitSizePar 62,
          bench "2^62-Non-Parallel" $ whnf preFactoredNumOfBitSize 62,
          bench "Regular-500" $ whnf genARandomPreFactoredNumberLTEn 500
        ]
    ]
