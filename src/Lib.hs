{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing this math function
module Lib
  ( genARandomPreFactoredNumberLTEn,
    createBasicSeq,
  )
where

-- import Data.Bool (bool) -- after refactoring yet again, using Data.bool.HT this is no longer being used
import Data.Bool.HT (if')
import Data.Numbers.Primes (isPrime)
-- import ShortCircuit (if') -- now after refactoring using bool, this is not being used

-- disabling termination checking
import Data.Text (pack)
import Protolude hiding (bool, die)
import RefinementHelper
import System.Random.Stateful (globalStdGen, uniformRM)

{-@ lazy createBasicSeq @-}
--{-@ createBasicSeq :: n:Pos -> IO (Either Text {o:[Pos] | True == descPosList o }) @-}
{-@ createBasicSeq :: n:Pos -> IO (Either Text [Pos]) @-}

-- | Provided an Int, creates a sequence of random integers LTE n decreasing with multiples ending at 1
createBasicSeq :: Int -> IO (Either Text [Int])
createBasicSeq x | x <= 0 = pure $ Left $ pack "Invalid"
createBasicSeq 1  = pure $ Right [1]
createBasicSeq n | n >= 2 = do
  seed <- getRndMInt (1, n)
  x <- createBasicSeq seed
  case x of
    Left _ -> pure $ Left $ pack "Invalid"
    Right nxt -> pure $ Right (seed : nxt)
createBasicSeq _ = pure $ Left $ pack "Invalid"

{-@ lazy genARandomPreFactoredNumberLTEn @-}
-- disabling termination checking

-- | This is the Entry Function.
-- Provide an integer input and it should generate a tuple of a number less than the integer i/p and its factors
genARandomPreFactoredNumberLTEn :: Int -> IO (Either Text (Int, [Int]))
genARandomPreFactoredNumberLTEn x | x <= 0 = pure $ Left $ pack "Invalid"
genARandomPreFactoredNumberLTEn 1 = pure $ Right (1, [1])
genARandomPreFactoredNumberLTEn n | n >= 2 = do
  m <- createBasicSeq n
  case m of
    Left _ -> pure $ Left $ pack "Invalid"
    Right seqNumbers -> if' (ps <= n) (pure $ Right rsp) (genARandomPreFactoredNumberLTEn n)
      where
        rsp@(ps, sq) = (product sq, 1 : [y | y <- filter isPrime seqNumbers, y > 0]) -- product [] is 1, surprising
genARandomPreFactoredNumberLTEn _ = pure $ Left $ pack "Invalid"

-- helper functions
{-@ getRndMInt :: x:{(Pos, Pos) | fst x <= snd x && fst x > 0} -> IO {y:Pos | y >= fst x && y <= snd x} @-}
-- Get a random integer given a lower and upper bound

-- | Get a Random Integer with uniform probability in the range [1,n]
getRndMInt :: (Int, Int) -> IO Int
getRndMInt (l, u) | l <= u && l > 0 = do
  result <- uniformRM (l, u) globalStdGen :: IO Int
  pure $ result `min` u `max` l
getRndMInt (l, u) | (l <= 0 || u <= 0) || (l > u) = die "impossible"
getRndMInt (l, u) | l < 0 && u < 0 = die "impossible"
getRndMInt _ = die "impossible"
