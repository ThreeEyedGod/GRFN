{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing this math function
module Lib
  ( genARandomPreFactoredNumberLTEn',
    getRndMInt,
    makeList
  )
where

-- import Data.Bool (bool) -- after refactoring yet again, using Data.bool.HT this is no longer being used
-- now after refactoring using bool, this is not being used

-- import Control.Conditional (ifM, (<&&>))
-- import Data.Bool.HT (if')

import Control.Applicative ((<*>))
import Data.Numbers.Primes (isPrime)
import Data.Text (pack)
import Protolude hiding (bool, die, getLine, head, ifM, trace, traceM)
import RefinementHelper
import System.Random.Stateful (globalStdGen, uniformRM)
import Prelude (String)

{-@ getRndMInt :: x:{(Pos, Pos) | fst x <= snd x && fst x > 0} -> IO {y:Pos | y >= fst x && y <= snd x} @-}

-- | Get a Random Integer with uniform probability in the range [1,n]
getRndMInt :: (Int, Int) -> IO Int
getRndMInt (l, u) | l <= u && l > 0 = max l . min u <$> uniformRM (l, u) globalStdGen
getRndMInt _ = die "impossible"

-- makeList n | n > 1 = do
--   seed <- getRndMInt (1, n) -- this gets int from IO Int from int
--   nxt <- makeList seed -- this gets [int] from IO [Int] from int
--   pure $ seed : nxt -- this gets IO [Int] from [int] from int and [int]
{-@ makeList :: n:Pos -> IO [RngPos 1 n] @-}
{-@ lazy makeList @-}

-- | Provided an Int, creates a sequence of random integers LTE n decreasing possibly with multiples ending at single 1
makeList :: Int -> IO [Int]
makeList 1 = pure [] -- pure [] also works
makeList n | n > 1 = (getRndMInt >=>: makeList) (1, n)
makeList _ = die "impossible"

infixr 1 >=>:
-- | Left-to-right Kleisli composition of monads plus prepend elem using standard operators
(>=>:) :: (Monad m) => (a -> m b) -> (b -> m [b]) -> (a -> m [b])
f >=>: g = f >=> \u -> (u :) <$> g u


{-@ lazy genARandomPreFactoredNumberLTEn' @-}

-- | This is the Entry Function.
-- Provide an integer input and it should generate a tuple of a number less than the integer i/p and its prime factors
genARandomPreFactoredNumberLTEn' :: Int -> IO (Either Text (Int, [Int]))
genARandomPreFactoredNumberLTEn' x | x <= 0 = pure $ Left $ pack "Invalid"
genARandomPreFactoredNumberLTEn' 1 = pure $ Right (1, [1])
genARandomPreFactoredNumberLTEn' n = makeList n >>= haltOrContinue n
  where
    haltOrContinue n' solnSet
      | ps <= n' = pure $ Right result
      | otherwise = genARandomPreFactoredNumberLTEn' n' -- keep doing till result occurs
      where
        result@(ps, sq) = (product sq, filter isPrimeOr1 solnSet) -- note: product [] = 1

-- | let it be true if it is prime or the integer 1

{-@ isPrimeOr1 :: Pos -> Bool @-}
isPrimeOr1 :: Int -> Bool
isPrimeOr1 n | n < 1 = die "impossible"
isPrimeOr1 n = (n == 1) || isPrime n