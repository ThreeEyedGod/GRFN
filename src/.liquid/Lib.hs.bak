{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing this math function
module Lib
  ( genARandomPreFactoredNumberLTEn,
  )
where

import Data.Numbers.Primes (isPrime)
import Data.Text (pack)
import Protolude hiding (die)
import RefinementHelper
import System.Random.Stateful (globalStdGen, uniformRM)

{-@ lazy genARandomPreFactoredNumberLTEn @-}
{-@ genARandomPreFactoredNumberLTEn :: n:Pos -> IO (EitherTupleIntListFactors n) @-}
-- promise

-- | This is the Entry Function.
-- Provide an integer input and it should generate a tuple of a number less than the input integer and its prime factors
genARandomPreFactoredNumberLTEn :: Int -> IO (Either Text (Int, [Int]))
genARandomPreFactoredNumberLTEn x | x <= 0 = pure $ Left $ pack "Invalid"
genARandomPreFactoredNumberLTEn 1 = pure $ Right (1, [1])
genARandomPreFactoredNumberLTEn n = makeList n >>= haltOrContinue n
  where
    haltOrContinue n' solnSet
      | ps <= n' = pure $ Right result
      | otherwise = genARandomPreFactoredNumberLTEn n' -- keep doing till result occurs
      where
        result@(ps, sq) = (product sq, filter isPrimeOr1 solnSet) -- note: product [] = 1

{-@ makeList :: n:Pos -> IO [RngPos 1 n] @-}
{-@ lazy makeList @-}

-- | Provided an Int, creates a sequence of random integers LTE n decreasing order, possibly with multiples ending at single 1
makeList :: Int -> IO [Int]
makeList 1 = pure [] -- pure [] also works
makeList n | n > 1 = (getRndMInt >=>: makeList) (1, n)
makeList _ = die "impossible"

{-@ getRndMInt :: x:{(Pos, Pos) | fst x <= snd x && fst x > 0} -> IO {y:Pos | y >= fst x && y <= snd x} @-}

-- | Get a Random Integer with uniform probability in the range [1,n]
getRndMInt :: (Int, Int) -> IO Int
getRndMInt (l, u) | l <= u && l > 0 = max l . min u <$> uniformRM (l, u) globalStdGen
getRndMInt _ = die "impossible"

infixr 1 >=>:

-- | Left-to-right Kleisli composition of monads plus prepend elem to List using standard operators
(>=>:) :: (Monad m) => (a -> m b) -> (b -> m [b]) -> (a -> m [b])
f >=>: g = f >=> \u -> (u :) <$> g u

-- | let it be true if it is prime or the integer 1

{-@ isPrimeOr1 :: Pos -> Bool @-}
isPrimeOr1 :: Int -> Bool
isPrimeOr1 n | n < 1 = die "impossible"
isPrimeOr1 n = (n == 1) || isPrime n
