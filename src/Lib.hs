{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing this math function
module Lib
  ( genARandomPreFactoredNumberLTEn',
    getRndMInt,
    makeList,
  )
where

-- import Data.Bool (bool) -- after refactoring yet again, using Data.bool.HT this is no longer being used

--import ShortCircuit (if', (??)) -- now after refactoring using bool, this is not being used

-- disabling termination checking

import Control.Monad (replicateM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Parallel
import Data.Bool.HT (if')
import Data.Ix (inRange)
import Data.List.Index (indexed)
import Data.Numbers.Primes (isPrime)
import Data.Semigroup ((<>))
import Data.Text (pack)
import Debug.Trace (trace, traceM)
import Protolude hiding (bool, die, head, trace, traceM)
import RefinementHelper
import System.Random.Stateful (globalStdGen, randomRIO, uniformRM)
import Prelude (String, head, tail, (!!))

{-@ getRndMInt :: x:{(Pos, Pos) | fst x <= snd x && fst x > 0} -> IO {y:Pos | y >= fst x && y <= snd x} @-}

-- | Get a Random Integer with uniform probability in the range [1,n]
getRndMInt :: (Int, Int) -> IO Int
getRndMInt (l, u) | l <= u && l > 0 = do
  result <- uniformRM (l, u) globalStdGen
  pure $ result `min` u `max` l
getRndMInt _ = die "impossible"


{-@ makeList :: n:Pos -> IO [RngPos 1 n] @-}
{-@ lazy makeList @-}

-- | Provided an Int, creates a sequence of random integers LTE n decreasing possibly with multiples ending at single 1
makeList :: Int -> IO [Int]
makeList 1 = pure [1] -- pure [] also works
makeList n | n >= 1 = do
  seed <- getRndMInt (1, n) -- int becomes IO Int becomes int
  fmap (seed :) (makeList seed)
makeList _ = die "impossible"

{-@ genARandomPreFactoredNumberLTEn' :: n:Int -> IO (Either Text (Pos, [RngPrimes 1 n])) @-}
{-@ lazy genARandomPreFactoredNumberLTEn' @-}

-- | This is the Entry Function.
-- Provide an integer input and it should generate a tuple of a number less than the integer i/p and its factors
genARandomPreFactoredNumberLTEn' :: Int -> IO (Either Text (Int, [Int]))
genARandomPreFactoredNumberLTEn' x | x <= 0 = pure $ Left $ pack "Invalid"
genARandomPreFactoredNumberLTEn' 1 = pure $ Right (1, [1])
genARandomPreFactoredNumberLTEn' n = do
  solnSet <- makeList n
  let rsp@(ps, sq) = (product sq, filter isPrime solnSet)
  if' (ps <= n) (pure $ Right rsp) (genARandomPreFactoredNumberLTEn' n)
genARandomPreFactoredNumberLTEn' _ = pure $ Left $ pack "Invalid"
