{-- |
-- Module      : Lib
-- Description : Implementing Adam Kalai's easier algo for getting uniform pre-factored integers
-- Copyright   : (c) VN, 2024
-- License     : GPL-3
-- Maintainer  : ThreeEyedGod
-- Stability   : Experimental
-- Portability : MacOS, Windows, Ubuntu
--
-- @uses LiquidHaskell, Applicatives@.
-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing this math function
module Lib
  ( genARandomPreFactoredNumberLTEn,
    preFactoredNumOfBitSize,
    preFactoredNumOfBitSizePar,
  )
where

import Control.Concurrent.ParallelIO.Local
import Control.Monad.Loops (iterateWhile)
import Data.Ix (inRange)
import Data.Maybe (fromJust)
import Data.Numbers.Primes (isPrime)
import Data.Text (pack)
import Data.Time.Clock
import GHC.Conc (getNumProcessors)
import Protolude hiding (die)
import RefinementHelper
import System.Random.Stateful (globalStdGen, uniformRM)

{-@ ignore preFactoredNumOfBitSizePar @-}
-- {-@ preFactoredNumOfBitSizePar :: n:Pos -> IO (EitherTupleIntListFactors n) @-}

-- | This is one of three Entry Functions. Takes an Int for Bitsize value.  This function leverages parallel execution
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors
preFactoredNumOfBitSizePar :: Int -> IO (Either Text (Int, [Int]))
preFactoredNumOfBitSizePar n | n <= 0 = pure $ Left $ pack "Invalid"
preFactoredNumOfBitSizePar 1 = pure $ Right (1, [1])
preFactoredNumOfBitSizePar n | n > 1 = fromJust <$> (getNumProcessors >>= spinUpNThreads preFactoredNumOfBitSize)
preFactoredNumOfBitSizePar _ = pure $ Left $ pack "Invalid"

{-@ ignore spinUpNThreads @-}
spinUpNThreads :: (Int -> IO a) -> Int -> IO (Maybe a)
spinUpNThreads f n = withPool n $ \pool -> parallelFirst pool $ replicate n (Just <$> f n)

{-@ ignore preFactoredNumOfBitSize @-}
-- {-@ preFactoredNumOfBitSize :: n:Pos -> IO (EitherTupleIntListFactors n) @-}

-- | This is one of three Entry Functions. Takes an Int as a Bitsize value.
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors
preFactoredNumOfBitSize :: Int -> IO (Either Text (Int, [Int]))
preFactoredNumOfBitSize n | n <= 0 = pure $ Left $ pack "Invalid"
preFactoredNumOfBitSize 1 = pure $ Right (1, [1])
preFactoredNumOfBitSize n | n > 1 = iterateWhile ((2 ^ n) ^|) (genARandomPreFactoredNumberLTEn (2 ^ (n + 1) - 1))
preFactoredNumOfBitSize _ = pure $ Left $ pack "Invalid"

infix 1 ^|

-- | An operator to compare the Right first value of the tuple to another for lesser than truthiness
(^|) :: Int -> Either Text (Int, [Int]) -> Bool
bound ^| eOR = case eOR of
  Left _ -> False
  Right v -> fst v < bound

{-@ lazy genARandomPreFactoredNumberLTEn @-}
{-@ genARandomPreFactoredNumberLTEn :: n:Pos -> IO (EitherTupleIntListFactors n) @-}
-- contract for this function

-- | This is the Entry Function with a Int bound
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

-- | True if it is prime or the Int 1

{-@ isPrimeOr1 :: Pos -> Bool @-}
isPrimeOr1 :: Int -> Bool
isPrimeOr1 n | n < 1 = die "impossible"
isPrimeOr1 n = (n == 1) || isPrime n

-- | Helper function that can be used to time execution
timeit :: IO a -> IO (Maybe a, NominalDiffTime)
timeit action = do
  start <- getCurrentTime
  value <- action
  end <- getCurrentTime
  return (Just value, (diffUTCTime end start))
