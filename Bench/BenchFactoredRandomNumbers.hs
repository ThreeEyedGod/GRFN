import FactoredRandomNumbers
import Test.Tasty.Bench

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain
    [ bgroup
        "PreFactored Uniform Random Numbers -Integers"
        [ bench "2^92-Parallel+Concurrent " $ whnfIO (preFactoredNumOfBitSizePar 92),
          bench "2^92-Parallel+Concurrent " $ whnfIO (preFactoredNumOfBitSize 92)
        ],
      bgroup
        "PreFactored Uniform Random Numbers -Max Int"
        [ bench "2^62-Parallel+Concurrent " $ whnfIO (preFactoredNumOfBitSizePar 62),
          bench "2^62-Parallel+Concurrent " $ whnfIO (preFactoredNumOfBitSize 62),
          bench "10^8 Parallel " $ whnfIO (genARandomPreFactoredNumberLTEn (10 ^ 8))
        ]
    ]
