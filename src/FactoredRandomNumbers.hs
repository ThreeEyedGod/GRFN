{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}
{-- |
-- Module: FactoredRandomNumbers
-- Description: A module of Kalai's algorithm to get uniformly pre-factored numbers
-- Copyright: (c) Venkatesh Narayanan
-- License:
-- Maintainer: venkatesh.narayanan@live.in
--
-- The module has three functions.
   The Adam Kalai Algorithm implmemented in this module (see Readme for more details)
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
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.ParallelIO.Local (parallelFirst, withPool)
import Control.Monad.Loops (iterateWhile)
import Control.Parallel.Strategies (NFData, parBuffer, parListChunk, parListSplitAt, rdeepseq, rpar, withStrategy)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Unamb (race)
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities)
import Math.NumberTheory.Primes.Testing (bailliePSW) -- isPrime is slower
import Protolude
  ( Applicative (pure),
    Bool (False),
    Either (..),
    Eq ((==)),
    Foldable (maximum),
    IO,
    Int,
    Integer,
    Maybe (Just),
    Monad ((>>=)),
    Num ((+), (-)),
    Ord (max, min, (<), (<=), (>)),
    Text,
    abs,
    div,
    filter,
    flip,
    fst,
    length,
    maximum,
    odd,
    otherwise,
    product,
    replicate,
    snd,
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
import System.Random.Stateful (globalStdGen, uniformRM)

-- | Strategies that may be used with parallel calls
data Strats
  = Chunk
  | Buffer
  | Split
  deriving (Eq)

-- | Takes an Integer for Bitsize value to operate on range [2 ^ y, 2 ^ y + 1 - 1].  This function leverages parallel execution
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors
-- In the event that the concurrent call fails, a recovery through a basic parallelised call is attempted.
preFactoredNumOfBitSizePar :: Integer -> IO (Either Text (Integer, [Integer]))
preFactoredNumOfBitSizePar 1 = pure $ Right (1, [1])
preFactoredNumOfBitSizePar n | n > 1 = fromMaybe <$> preFactoredNumOfBitSize n <*> preFactoredNumOfBitSizeParMaybe n
preFactoredNumOfBitSizePar _ = pure $ Left $ pack "Invalid"

-- | Parallel preFactored Number given BitSize
-- Provide an integer input. Generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors.
preFactoredNumOfBitSizeParMaybe :: Integer -> IO (Maybe (Either Text (Integer, [Integer])))
preFactoredNumOfBitSizeParMaybe 1 = pure $ Just $ Right (1, [1])
preFactoredNumOfBitSizeParMaybe n | n > 1 && n < (10 :: Integer) ^ (9 :: Integer) = Just <$> preFactoredNumOfBitSize n
preFactoredNumOfBitSizeParMaybe n | n > 1 = (snd <$> coresToUse) >>= spinUpThreads (preFactoredNumOfBitSize n)
preFactoredNumOfBitSizeParMaybe _ = pure $ Just $ Left $ pack "Invalid"

-- | Spin up t threads of function f in parallel and return the one which completes first
spinUpThreads :: IO a -> Int -> IO (Maybe a)
spinUpThreads f t = withPool t $ \pool -> parallelFirst pool $ replicate t (Just <$> f)

-- | Spin up t actions of function f in parallel and return what's executed first
-- for now ignore t; fires up a 2 horse 'race' call from Control.Concurrent.Async
_spinUpActions :: IO a -> Int -> IO (Maybe a)
_spinUpActions f _ = _raceJust f

-- | Spin up t Forks of function f in parallel and return what's executed first
-- for now ignore the second paramemter; fires up a 2-horse 'race' call from Data.Unamb
_spinUpForks :: IO a -> Int -> IO (Maybe a)
_spinUpForks f _ = _raceJustU f

-- | Convert async.race from Either-Or to Maybe
_raceJust :: IO a -> IO (Maybe a)
_raceJust a =
  race a a >>= \case
    Left u -> pure $ Just u
    Right v -> pure $ Just v

-- | Convert Data.Unamb.race to Maybe
_raceJustU :: IO a -> IO (Maybe a)
_raceJustU a = Just <$> Data.Unamb.race a a

-- | Figure out # cores to use for parallelization
coresToUse :: IO (Int, Int)
coresToUse = do
  nCores <- getNumProcessors
  let nEfficiencyCores = nCores `div` 2 -- a heuristic : 50% of cores = efficiency
  nNumCapabilities <- getNumCapabilities
  setNumCapabilities $ max (nCores - nEfficiencyCores) nNumCapabilities
  nNumCapabilitiesSet <- getNumCapabilities
  pure (nCores, nNumCapabilitiesSet)

-- | Takes an Integer as a Bitsize value to operate on range [2 ^ y, 2 ^ y + 1 - 1]
-- Provide an integer input and it should generate a tuple of a number in the range [2^y, 2^y+1 -1] and its prime factors.
-- if it throws up a value below 2^n then do again. 50% of the time it should result in success.
preFactoredNumOfBitSize :: Integer -> IO (Either Text (Integer, [Integer]))
preFactoredNumOfBitSize 1 = pure $ Right (1, [1])
preFactoredNumOfBitSize n | n > 1 = iterateWhile ((2 ^ n) <|) (genARandomPreFactoredNumberLTEn (2 ^ (n + 1) - 1))
preFactoredNumOfBitSize _ = pure $ Left $ pack "Invalid"

infix 1 <|

-- | An operator to compare the Right first value of the (Int, [Int]) to an Int for Truth-value of lesser-than predicate
(<|) :: Integer -> Either Text (Integer, [Integer]) -> Bool
bound <| eOR = case eOR of
  Left _ -> False
  Right v -> fst v < bound

-- | This is the Entry Function with a Integer bound. This is the core of the Kalai algorithm
-- Provide an integer input and it should generate a tuple of a number less than the input integer and its prime factors
genARandomPreFactoredNumberLTEn :: Integer -> IO (Either Text (Integer, [Integer]))
genARandomPreFactoredNumberLTEn x | x <= 0 = pure $ Left $ pack "Invalid"
genARandomPreFactoredNumberLTEn 1 = pure $ Right (1, [1])
genARandomPreFactoredNumberLTEn n = do
  candidateTuple <- potentialResult n
  if' ((fst `is` (< n)) candidateTuple) (pure $ Right candidateTuple) (genARandomPreFactoredNumberLTEn n) -- else keep doing till success

-- | Provided an Integer List, throws up a candidate Int and its prime factors for further assessment
filterPrimesProduct :: [Integer] -> (Integer, [Integer])
filterPrimesProduct xs = result where result@(_, sq) = (product sq, onlyPrimesFrom xs) -- note: product [] = 1

-- | parallel filter with 3 optional strategies
parFilter :: (NFData a) => Strats -> Int -> (a -> Bool) -> [a] -> [a]
parFilter strat stratParm p = case strat of
  Chunk -> withStrategy (parListChunk stratParm rdeepseq) . filter p
  Buffer -> withStrategy (parBuffer stratParm rpar) . filter p
  Split -> withStrategy (parListSplitAt stratParm rdeepseq rdeepseq) . filter p

-- | Reduction of a composite list of integers into primefactors
-- Select the parallel (or not) strategy based on the size range. Use Parallel > Billion
onlyPrimesFrom :: [Integer] -> [Integer]
onlyPrimesFrom xs
  | maximum xs < (10 :: Integer) ^ (9 :: Integer) = filter isPrimeOr1 xs -- at a billion try parallelzing and concurrency options
  | otherwise = parFilter Split (length xs `div` 3) isPrimeOr1 xs

-- assuming the first 3rd of the list comprise larger numbers and their processing workload = the residual 2/3rd

-- | Provided an Integer, throws up a candidate Int and its factors for further evaluation
potentialResult :: Integer -> IO (Integer, [Integer])
potentialResult n = mkList n <&> filterPrimesProduct

-- | Provided an Integer, creates a sequence of random integers LTE n in decreasing order,
-- possibly with multiples ending at 1
mkList :: Integer -> IO [Integer]
mkList 1 = pure []
mkList n = (getRndMInt >=>: mkList) (1, n)

-- | Get a Random Integer with uniform probability in the range (l,u)
getRndMInt :: (Integer, Integer) -> IO Integer
getRndMInt (l, u) = max l . min u <$> uniformRM (l, u) globalStdGen -- uniformRM (a, b) = uniformRM (b, a) holds as per fn defn

infixr 1 >=>:

-- | Left-to-right Kleisli composition of monads plus prepend elem to List using applicative
-- Late edit: there may be something in Control.arrow that already does exactly this
(>=>:) :: (Monad m) => (a -> m b) -> (b -> m [b]) -> (a -> m [b])
f >=>: g = f >=> \u -> (u :) <$> g u

-- | True if input is prime or 1
-- Primality testing is one key to peformance of this algo
-- the isOdd and greater than 3 is for the use of bailliePSW primality
-- using bailliePSW in place of the standard isPrime leads to 75% reduction in time !!!
isPrimeOr1 :: Integer -> Bool
isPrimeOr1 n = (i == 1 || i == 3) || (i > 3 && odd i && bailliePSW i) where i = abs n -- bailliePSW requires that n > 3 and that input is Odd

-- | from Data.Function.predicate
is :: (a -> b) -> (b -> Bool) -> (a -> Bool)
is = flip (.)

-- | @if then else@ made concise
if' :: Bool -> b -> b -> b
if' p u v
  | p = u
  | otherwise = v
