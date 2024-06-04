{-# LANGUAGE UnicodeSyntax #-}
{--
   The Adam Kalai Algorithm implmemented in this module
              Input: Integer n > 0.

              Output: A uniformly random number 1 ≤ r ≤ n.

                  1. Generate a sequence n ≥ s1 ≥ s2 ≥ ··· ≥ sl = 1 by choosing
                  s1 ∈ {1, 2,..., n} and si+1 ∈ {1, 2,...,si}, until reaching 1.
                  2. Let r be the product of the prime si’s.
                  3. If r ≤ n, output r with probability r/n.
                  4. Otherwise, RESTART.
--}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing functions based on Kalai's Algorithm
module FactoredRandomNumbers
  ( genARandomPreFactoredNumberLTEn,
    preFactoredNumOfBitSize,
    preFactoredNumOfBitSizePar,
    preFactoredNumOfBitSizeParMaybe,
  )
where

import Control.Concurrent.ParallelIO.Local (parallelFirst, withPool)
import Control.Monad.Loops (iterateWhile)
import Data.Maybe (fromMaybe)
import Data.Numbers.Primes (isPrime)
import Data.Text (pack)
import GHC.Conc (getNumProcessors)
import Protolude
  ( Applicative (pure),
    Bool (False),
    Either (..),
    Eq ((==)),
    IO,
    Int,
    Maybe (Just),
    Monad ((>>=)),
    Num ((+), (-)),
    Ord (max, min, (<), (<=), (>)),
    Text,
    filter,
    flip,
    fst,
    product,
    replicate,
    ($),
    (&&),
    (.),
    (<$>),
    (<&>),
    (<*>),
    (>=>),
    (^),
    (||),
  )
import ShortCircuit (if')
import System.Random.Stateful (globalStdGen, uniformRM)
import Prelude (error)

-- | Takes an Int for Bitsize value to operate on range [2 ^ y, 2 ^ y + 1 - 1].  This function leverages parallel execution
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors
-- In the event that the parallel call fails and return Nothing, a recovery through a non-parallel call is attempted.
preFactoredNumOfBitSizePar :: Int -> IO (Either Text (Int, [Int]))
preFactoredNumOfBitSizePar 1 = pure $ Right (1, [1])
preFactoredNumOfBitSizePar n | n > 1 = fromMaybe <$> preFactoredNumOfBitSize n <*> preFactoredNumOfBitSizeParMaybe n
preFactoredNumOfBitSizePar _ = pure $ Left $ pack "Invalid"

-- | Failable Parallel preFactored Number given BitSize
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors
preFactoredNumOfBitSizeParMaybe :: Int -> IO (Maybe (Either Text (Int, [Int])))
preFactoredNumOfBitSizeParMaybe 1 = pure $ Just $ Right (1, [1])
preFactoredNumOfBitSizeParMaybe n | n > 1 = getNumProcessors >>= spinUpThreads (preFactoredNumOfBitSize n)
preFactoredNumOfBitSizeParMaybe _ = pure $ Just $ Left $ pack "Invalid"

-- | Spin up t threads of function f in parallel and return what's executed first
spinUpThreads :: IO a -> Int -> IO (Maybe a)
spinUpThreads f t = withPool t $ \pool -> parallelFirst pool $ replicate t (Just <$> f)

-- | Takes an Int as a Bitsize value to operate on range [2 ^ y, 2 ^ y + 1 - 1]
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors
preFactoredNumOfBitSize :: Int -> IO (Either Text (Int, [Int]))
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
genARandomPreFactoredNumberLTEn :: Int -> IO (Either Text (Int, [Int]))
genARandomPreFactoredNumberLTEn x | x <= 0 = pure $ Left $ pack "Invalid"
genARandomPreFactoredNumberLTEn 1 = pure $ Right (1, [1])
genARandomPreFactoredNumberLTEn n = do
  candidateTuple <- potentialResult n
  if' ((fst `is` (< n)) candidateTuple) (pure $ Right candidateTuple) (genARandomPreFactoredNumberLTEn n) -- keep doing till success

-- | Provided an Int List, throws up a candidate Int and its prime factors for further assessment
filterPrimesProduct :: [Int] -> (Int, [Int])
filterPrimesProduct xs = result where result@(ps, sq) = (product sq, filter isPrimeOr1 xs) -- note: product [] = 1

-- | Provided an Int, throws up a candidate Int and its factors for further assessment
potentialResult :: Int -> IO (Int, [Int])
potentialResult n = makeList n <&> filterPrimesProduct

-- | Provided an Int, creates a sequence of random integers LTE n in decreasing order,
-- possibly with multiples ending at a single 1
makeList :: Int -> IO [Int]
makeList 1 = pure []
makeList n | n > 1 = (getRndMInt >=>: makeList) (1, n)
makeList _ = error "Out of bound Arg"

-- | Get a Random Integer with uniform probability in the range [1,n]
getRndMInt :: (Int, Int) -> IO Int
getRndMInt (l, u) | l <= u && l > 0 = max l . min u <$> uniformRM (l, u) globalStdGen
getRndMInt _ = error "Malformed Range"

infixr 1 >=>:

-- | Left-to-right Kleisli composition of monads plus prepend elem to List using standard operators
(>=>:) :: (Monad m) => (a -> m b) -> (b -> m [b]) -> (a -> m [b])
f >=>: g = f >=> \u -> (u :) <$> g u

-- | True if input is prime or 1
isPrimeOr1 :: Int -> Bool
isPrimeOr1 n | n > 0 = (n == 1) || isPrime n
isPrimeOr1 _ = error "Invalid Arg "

-- | from Data.Function.predicate
is :: (a -> b) -> (b -> Bool) -> (a -> Bool)
is = flip (.)
