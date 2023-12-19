{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( genARandomPreFactoredNumberLEn,
    firstPrimeLE,
    createSeq,
  )
where

import Data.Numbers.Primes (isPrime)
import Protolude hiding (die)
import RefinementHelper
import ShortCircuit (if')
import System.Random.Stateful (globalStdGen, uniformRM)
import Prelude (String)
import Data.Text (pack)

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
createSeq n | n >= 2 = case filterInvalidNonPos n of
  Left _ -> createSeq 1
  Right nGte1 -> do
    case filterInvalidNonPos (si - 1) of
      Left _ -> createSeq 1
      Right okN -> si : createSeq okN
    where
      si = firstPrimeLE nGte1
createSeq _ = die "impossible"

-- {-@ lazy firstPrimeLE @-} -- disabling termination checking
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

