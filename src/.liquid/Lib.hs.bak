{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( genARandomPreFactoredNumberLEn,
    firstPrimeLE,
    createSeq,
  )
where

import Data.Numbers.Primes (isPrime)
import Data.Text (pack)
import Protolude hiding (die, (||))
import RefinementHelper
import ShortCircuit (if')
import System.Random.Stateful (globalStdGen, uniformRM)

{-@ lazy genARandomPreFactoredNumberLEn @-}
-- disabling termination checking
genARandomPreFactoredNumberLEn :: Int -> IO (Either Text (Int, [Int]))
genARandomPreFactoredNumberLEn x | x <= 0 = pure $ Left $ pack "Invalid"
genARandomPreFactoredNumberLEn 1 = pure $ Right (1, [1])
genARandomPreFactoredNumberLEn n | n >= 2 = do
  rndM <- fmap filterInvalid (getRndMInt (2, n))
  case rndM of
    Left _ -> pure $ Left $ pack "Invalid"
    Right upper -> if' (ps <= n) (pure $ Right rsp) (genARandomPreFactoredNumberLEn n) -- if' from shortcircuit, used here for convenience not lazy evaluation
      where
        rsp@(ps, sq) = (product sq, createSeq upper) -- Haskell as-pattern @
genARandomPreFactoredNumberLEn _ = pure $ Left $ pack "Invalid"

{-@ lazy createSeq @-}
-- disabling termination checking
{-@ createSeq :: Pos -> PrimeFactors @-}
createSeq :: Int -> [Int]
createSeq 1 = [1]
createSeq n | n >= 2 = case filterInvalidNonPos $ firstPrimeLE n of
  Left _ -> createSeq 1
  Right okN -> lstPrimesLE okN
createSeq _ = die "impossible"

{-@ lstPrimesLE :: Pos -> PrimeFactors @-}
lstPrimesLE :: Int -> [Int]
lstPrimesLE 1 = [1]
lstPrimesLE n | n >= 2 = 1 : [x | x <- [1 .. n], x > 0, isPrime x] -- the "1: " is there to 'prove' to the SMT solver that the len lst > 0 equivalent to the defn of PrimeFactors
lstPrimesLE _ = die "impossible"

-- {-@ lazy firstPrimeLE @-} -- disabling termination checking
-- {-@ firstPrimeLE :: Pos -> {v:Pos | v==1 || isPrime v} @-}
-- it would be nice to have the above refinement working; it's tighter on the output
{-@ firstPrimeLE :: Pos -> Pos @-}
firstPrimeLE :: Int -> Int
firstPrimeLE 1 = 1
firstPrimeLE n | isPrime n = n
firstPrimeLE n | n > 0 = firstPrimeLE (n - 1)
firstPrimeLE _ = die "impossible"

-- helper functions
-- get a random integer given a lower and upper bound
getRndMInt :: (Int, Int) -> IO Int
getRndMInt (l, u) = uniformRM (l, u) globalStdGen :: IO Int
