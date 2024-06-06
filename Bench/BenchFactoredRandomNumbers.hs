import FactoredRandomNumbers
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ 
      bgroup
        "PreFactored Uniform Random Numbers -Integers"
        [
          bench "2^92-parallel " $ whnfIO (preFactoredNumOfBitSizePar 92),
          bench "2^92-Non-Parallel " $ whnfIO (preFactoredNumOfBitSize 92),
          bench "Regular-500 " $ whnfIO (genARandomPreFactoredNumberLTEn 500)
        ],
      bgroup
        "PreFactored Uniform Random Numbers -Max Int"
        [
          bench "2^62-parallel " $ whnfIO (preFactoredNumOfBitSizePar 62),
          bench "2^62-Non-Parallel " $ whnfIO (preFactoredNumOfBitSize 62),
          bench "Regular-500 " $ whnfIO (genARandomPreFactoredNumberLTEn 500)
        ]
      -- bgroup
      --   "PreFactored Uniform Random Numbers -Median Ints"
      --   [ bench "2^62-parallel " $ whnfIO (preFactoredNumOfBitSizePar 32),
      --     bench "2^62-Non-Parallel " $ whnfIO (preFactoredNumOfBitSize 22),
      --     bench "Regular-500 " $ whnfIO (genARandomPreFactoredNumberLTEn 500)
      --   ]
      -- bgroup
      --   "PreFactored Uniform Random Numbers -Quick Small Ints"
      --   [ bench "2^6-parallel " $ whnfIO (preFactoredNumOfBitSizePar 6),
      --     bench "2^6-Non-Parallel " $ whnfIO (preFactoredNumOfBitSize 6),
      --     bench "Regular-50 " $ whnfIO (genARandomPreFactoredNumberLTEn 50)
      --   ]
    ]
