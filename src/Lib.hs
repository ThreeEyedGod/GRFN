{-# LANGUAGE NoImplicitPrelude #-}

-- | Module for accessing this math function
module Lib
  ( genARandomPreFactoredNumberLTEn,
    createBasicSeq,
    getRndMInt,
    makeList
  )
where

-- import Data.Bool (bool) -- after refactoring yet again, using Data.bool.HT this is no longer being used

-- import ShortCircuit (if') -- now after refactoring using bool, this is not being used

-- disabling termination checking

import Control.Monad (replicateM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Parallel
import Data.Bool.HT (if')
import Data.List.Index (indexed)
import Data.Numbers.Primes (isPrime)
import Data.Text (pack)
import Debug.Trace (trace, traceM)
import Protolude hiding (bool, die, head, trace, traceM)
import RefinementHelper
import System.Random.Stateful (globalStdGen, randomRIO, uniformRM)
import Prelude (head, (!!), tail, String)
import Data.Ix (inRange)
import Data.Semigroup ((<>))

{-@ lazy createBasicSeq @-}
{-@ createBasicSeq :: n:Pos -> IO (Either Text (LstPosMaxN 1 n)) @-}
-- {-@ createBasicSeq :: n:Pos -> IO (Either Text [Pos]) @-}

-- | Provided an Int, creates a sequence of random integers LTE n decreasing possibly with multiples ending at single 1
createBasicSeq :: Int -> IO (Either Text [Int])
createBasicSeq x | x <= 0 = pure $ Left $ pack "Invalid"
createBasicSeq 1 = pure $ Right []
createBasicSeq n | n >= 2 = do
  seed <- getRndMInt (1, n)
  x <- createBasicSeq seed
  case x of
    Left _ -> pure $ Left $ pack "Invalid"
    Right [] -> pure $ Left $ pack "Invalid"
    Right nxt@(x1 : _) -> do
      case (seed <= n && seed > 0 && x1 <= n && x1 > 0 && ((seed - x1) >= 0)) of
        True -> pure $ Right (seed : nxt)
        False -> pure $ Left $ pack "Invalid"
createBasicSeq _ = pure $ Left $ pack "Invalid"

{-@ lazy genARandomPreFactoredNumberLTEn @-}

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
  result <- uniformRM (l, u) globalStdGen
  pure $ result `min` u `max` l
getRndMInt _ = die "impossible"

{-@ ignore makeList @-}
makeList :: Int -> IO [Int]
makeList 1 = pure []
makeList n = do
  seed <- getRndMInt (1, n) -- int becomes IO Int becomes int
  fmap (seed :) (makeList seed)
