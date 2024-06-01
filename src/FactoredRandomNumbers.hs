{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing functions based on Kalai's Algorithm
module FactoredRandomNumbers
  ( genARandomPreFactoredNumberLTEn,
    preFactoredNumOfBitSize,
    preFactoredNumOfBitSizePar,
  )
where

import Control.Concurrent.ParallelIO.Local (parallelFirst, withPool)
import Control.Monad.Loops (iterateWhile)
import Data.Maybe (fromJust)
import Data.Numbers.Primes (isPrime)
import Data.Text (pack)
import Data.Time.Clock
import GHC.Conc (getNumProcessors)
import Protolude hiding (die)
import RefinementHelper
import System.Random.Stateful (globalStdGen, uniformRM)

-- | Takes an Int for Bitsize value to operate on range [2 ^ y, 2 ^ y + 1 - 1].  This function leverages parallel execution
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors

{-@ ignore preFactoredNumOfBitSizePar @-}
-- {-@ preFactoredNumOfBitSizePar :: n:Pos -> IO (EitherTupleIntListFactors n) @-}
preFactoredNumOfBitSizePar :: Int -> IO (Either Text (Int, [Int]))
preFactoredNumOfBitSizePar n | n <= 0 = pure $ Left $ pack "Invalid"
preFactoredNumOfBitSizePar 1 = pure $ Right (1, [1])
preFactoredNumOfBitSizePar n | n > 1 = fromJust <$> (getNumProcessors >>= spinUpThreads (preFactoredNumOfBitSize n))
preFactoredNumOfBitSizePar _ = pure $ Left $ pack "Invalid"

-- | Spin up t threads of function f in parallel and return what's executed first

spinUpThreads :: IO a -> Int -> IO (Maybe a)
spinUpThreads f t = withPool t $ \pool -> parallelFirst pool $ replicate t (Just <$> f)

-- | Takes an Int as a Bitsize value to operate on range [2 ^ y, 2 ^ y + 1 - 1]
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors

{-@ ignore preFactoredNumOfBitSize @-}

-- {-@ preFactoredNumOfBitSize :: n:Pos -> IO (EitherTupleIntListFactors n) @-}
preFactoredNumOfBitSize :: Int -> IO (Either Text (Int, [Int]))
preFactoredNumOfBitSize n | n <= 0 = pure $ Left $ pack "Invalid"
preFactoredNumOfBitSize 1 = pure $ Right (1, [1])
preFactoredNumOfBitSize n | n > 1 = iterateWhile ((2 ^ n) <|) (genARandomPreFactoredNumberLTEn (2 ^ (n + 1) - 1))
preFactoredNumOfBitSize _ = pure $ Left $ pack "Invalid"

infix 1 <|

-- | An operator to compare the Right first value of the (Int, [Int]) to an Int for lesser-than predicate
(<|) :: Int -> Either Text (Int, [Int]) -> Bool
bound <| eOR = case eOR of
  Left _ -> False
  Right v -> fst v < bound

-- | This is the Entry Function with a Int bound
-- Provide an integer input and it should generate a tuple of a number less than the input integer and its prime factors

{-@ lazy genARandomPreFactoredNumberLTEn @-}
{-@ genARandomPreFactoredNumberLTEn :: n:Pos -> IO (EitherTupleIntListFactors n) @-}
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

-- | Provided an Int, creates a sequence of random integers LTE n decreasing order, possibly with multiples ending at single 1

{-@ makeList :: n:Pos -> IO [RngPos 1 n] @-}
{-@ lazy makeList @-}
makeList :: Int -> IO [Int]
makeList 1 = pure [] -- pure [] also works
makeList n | n > 1 = (getRndMInt >=>: makeList) (1, n)
makeList _ = die "impossible"

-- | Get a Random Integer with uniform probability in the range [1,n]

{-@ getRndMInt :: x:{(Pos, Pos) | fst x <= snd x && fst x > 0} -> IO {y:Pos | y >= fst x && y <= snd x} @-}
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

-- | Helper function
timeit :: IO a -> IO (Maybe a, NominalDiffTime)
timeit action = do
  start <- getCurrentTime
  value <- action
  end <- getCurrentTime
  pure (Just value, diffUTCTime end start)
